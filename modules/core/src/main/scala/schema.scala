// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.implicits._
import io.circe.Json

import QueryInterpreter.mkErrorResult
import ScalarType._

/**
 * Representation of a GraphQL schema
 *
 * A `Schema` is a collection of type and directive declarations.
 */
trait Schema {
  /** The types defined by this `Schema`. */
  val types: List[NamedType]
  /** The directives defined by this `Schema`. */
  val directives: List[Directive]

  /** A reference by name to a type defined by this `Schema`.
   *
   *  `TypeRef`s refer to types defined in this schema by name and hence
   *  can be used as part of mutually recursive type definitions.
   */
  def TypeRef(ref: String): TypeRef =
    new TypeRef(this, ref)

  /**
   * The default type of a GraphQL schema
   *
   * Unless a type named `"Schema"` is explicitly defined as part of
   * this `Schema` a definition of the form,
   *
   * ```
   * type Schema {
   *   query: Query!
   *   mutation: Mutation
   *   subscription: Subscription
   *  }
   * ```
   *
   * is used.
   */
  def defaultSchemaType = {
    val defaultQueryType = tpe("Query")
    val defaultMutationType = tpe("Mutation").nullable
    val defaultSubscriptionType = tpe("Subscription").nullable
    ObjectType(
      name = "Schema",
      description = None,
      fields = List(
        Field("query", None, Nil, defaultQueryType, false, None),
        Field("mutation", None, Nil, defaultMutationType, false, None),
        Field("subscription", None, Nil, defaultSubscriptionType, false, None)
      ),
      interfaces = Nil
    )
  }

  /**
   * Look up by name a type defined in this `Schema`.
   *
   * Yields the type, if defined, `NoType` otherwise.
   */
  def tpe(name: String): Type =
    types.find(_.name == name).getOrElse(Type.tpe(name))

  /**
   * The schema type.
   *
   * Either the explicitly defined type named `"Schema"` or the default
   * schema type if not defined.
   */
  def schemaType = tpe("Schema") orElse defaultSchemaType

  /** The type of queries defined by this `Schema` */
  def queryType: Type = schemaType.field("query")
  /** The type of mutations defined by this `Schema` */
  def mutationType: Type = schemaType.field("mutation")
  /** The type of subscriptions defined by this `Schema` */
  def subscriptionType: Type = schemaType.field("subscription")
}

/**
 * A GraphQL type definition.
 */
sealed trait Type {
  /**
   * Is this type equivalent to `other`.
   *
   * Note that plain `==` will distinguish types from type aliases,
   * which is typically not desirable, so `=:=` is usually the
   * most appropriate comparison operator.
   */
  def =:=(other: Type): Boolean = (this eq other) || (dealias == other.dealias)

  /** `true` if this type equivalent is a subtype of `other`. */
  def <:<(other: Type): Boolean =
    (this.dealias, other.dealias) match {
      case (ObjectType(_, _, _, interfaces), tp2) => interfaces.exists(_.dealias == tp2)
      case (tp1, UnionType(_, _, members))        => members.exists(tp1 <:< _)
      case _ => false
    }

  /**
   * This type if it isn't `NoType`, `other` otherwise.
   */
  def orElse(other: => Type): Type = this match {
    case NoType => other
    case _ => this
  }

  /**
   * Some of this type if it isn't `NoType`, `None` otherwise.
   */
  def toOption: Option[Type] = this match {
    case NoType => None
    case _ => Some(this)
  }

  /**
   * Yield the type of the field of this type named `fieldName` or
   * `NoType` if there is no such field.
   */
  def field(fieldName: String): Type = this match {
    case NullableType(tpe) => tpe.field(fieldName)
    case TypeRef(_, _) => dealias.field(fieldName)
    case ObjectType(_, _, fields, _) => fields.find(_.name == fieldName).map(_.tpe).getOrElse(NoType)
    case InterfaceType(_, _, fields) => fields.find(_.name == fieldName).map(_.tpe).getOrElse(NoType)
    case _ => NoType
  }

  /** `true` if this type has a field named `fieldName`, false otherwise. */
  def hasField(fieldName: String): Boolean =
    field(fieldName) != NoType

  /**
   * Yield the type of the field at the end of the path `fns` starting
   * from this type, or `NoType` if there is no such field.
   */
  def path(fns: List[String]): Type = (fns, this) match {
    case (Nil, _) => this
    case (_, ListType(tpe)) => tpe.path(fns)
    case (_, NullableType(tpe)) => tpe.path(fns)
    case (_, TypeRef(_, _)) => dealias.path(fns)
    case (fieldName :: rest, ObjectType(_, _, fields, _)) =>
      fields.find(_.name == fieldName).map(_.tpe.path(rest)).getOrElse(NoType)
    case (fieldName :: rest, InterfaceType(_, _, fields)) =>
      fields.find(_.name == fieldName).map(_.tpe.path(rest)).getOrElse(NoType)
    case _ => NoType
  }

  /**
   * Does the path `fns` from this type specify multiple values.
   *
   * `true` if navigating through the path `fns` from this type
   * might specify 0 or more values. This will be the case if the
   * path passes through at least one field of a List type.
   */
  def pathIsList(fns: List[String]): Boolean = (fns, this) match {
    case (Nil, _) => false
    case (_, _: ListType) => true
    case (_, NullableType(tpe)) => tpe.pathIsList(fns)
    case (_, TypeRef(_, _)) => dealias.pathIsList(fns)
    case (fieldName :: rest, ObjectType(_, _, fields, _)) =>
      fields.find(_.name == fieldName).map(_.tpe.pathIsList(rest)).getOrElse(false)
    case (fieldName :: rest, InterfaceType(_, _, fields)) =>
      fields.find(_.name == fieldName).map(_.tpe.pathIsList(rest)).getOrElse(false)
    case _ => false
  }

  /**
   * Does the path `fns` from this type specify a nullable type.
   *
   * `true` if navigating through the path `fns` from this type
   * might specify an optional value. This will be the case if the
   * path passes through at least one field of a nullable type.
   */
  def pathIsNullable(fns: List[String]): Boolean = (fns, this) match {
    case (Nil, _) => false
    case (_, ListType(tpe)) => tpe.pathIsNullable(fns)
    case (_, _: NullableType) => true
    case (_, TypeRef(_, _)) => dealias.pathIsNullable(fns)
    case (fieldName :: rest, ObjectType(_, _, fields, _)) =>
      fields.find(_.name == fieldName).map(_.tpe.pathIsNullable(rest)).getOrElse(false)
    case (fieldName :: rest, InterfaceType(_, _, fields)) =>
      fields.find(_.name == fieldName).map(_.tpe.pathIsNullable(rest)).getOrElse(false)
    case _ => false
  }

  /** Strip off aliases */
  def dealias: Type = this match {
    case TypeRef(schema, tpnme) => schema.types.find(_.name == tpnme).getOrElse(NoType)
    case _ => this
  }

  /** Is this type nullable? */
  def isNullable: Boolean = this match {
    case NullableType(_) => true
    case _ => false
  }

  /** This type if it is nullable, `Nullable(this)` otherwise. */
  def nullable: Type = this match {
    case NoType => NoType
    case t: NullableType => t
    case t => NullableType(t)
  }

  /**
   * A non-nullable version of this type.
   *
   * If this type is nullable, yield the non-nullable underlying
   * type. Otherwise yield this type.
   */
  def nonNull: Type = this match {
    case NullableType(tpe) => tpe.nonNull
    case _ => this
  }

  /** Is this type a list. */
  def isList: Boolean = this match {
    case ListType(_) => true
    case _ => false
  }

  /**
   * The element type of this type.
   *
   * If this type is is a list, yield the non-list underlying type.
   * Otherwise yield `NoType`.
   */
  def item: Type = this match {
    case NullableType(tpe) => tpe.item
    case ListType(tpe) => tpe
    case _ => NoType
  }

  /**
   * Yield the object type underlying this type.
   *
   * Strip off all aliases, nullability and enclosing list types until
   * an underlying object type is reached, in which case yield it, or a
   * non-object type which isn't further reducible is reached, in which
   * case yield `NoType`.
   */
  def underlyingObject: Type = this match {
    case NullableType(tpe) => tpe.underlyingObject
    case ListType(tpe) => tpe.underlyingObject
    case _: TypeRef => dealias.underlyingObject
    case o: ObjectType => o
    case i: InterfaceType => i
    case _ => NoType
  }

  /**
   * Yield the type of the field named `fieldName` of the object type
   * underlying this type.
   *
   * Strip off all aliases, nullability and enclosing list types until
   * an underlying object type is reached which has a field named
   * `fieldName`, in which case yield the type of that field; if there
   * is no such field, yields `NoType`.
   */
  def underlyingField(fieldName: String): Type = this match {
    case NullableType(tpe) => tpe.underlyingField(fieldName)
    case ListType(tpe) => tpe.underlyingField(fieldName)
    case TypeRef(_, _) => dealias.underlyingField(fieldName)
    case ObjectType(_, _, fields, _) => fields.find(_.name == fieldName).map(_.tpe).getOrElse(NoType)
    case InterfaceType(_, _, fields) => fields.find(_.name == fieldName).map(_.tpe).getOrElse(NoType)
    case _ => NoType
  }

  /** Is this type a leaf type?
   *
   * `true` if after stripping of aliases and nullability, is the underlying
   * type a scalar or an enum, `false` otherwise.
   */
  def isLeaf: Boolean = this match {
    case NullableType(tpe) => tpe.isLeaf
    case TypeRef(_, _) => dealias.isLeaf
    case _: ScalarType => true
    case _: EnumType => true
    case _ => false
  }

  /**
   * If the underlying type of this type is a scalar or an enum then yield it
   * otherwise yield `NoType`.
   */
  def asLeaf: Type = this match {
    case NullableType(tpe) => tpe.asLeaf
    case TypeRef(_, _) => dealias.asLeaf
    case _: ScalarType => this
    case _: EnumType => this
    case _ => NoType
  }

  /** Yield a String representation of this type */
  def describe: String

  /** Yield a short String representation of this type */
  override def toString: String = describe
}

object Type {
  import ScalarType._

  def tpe(tpnme: String): Type = tpnme match {
    case "Int" => IntType
    case "Float" => FloatType
    case "String" => StringType
    case "Boolean" => BooleanType
    case "ID" => IDType
    case _ => NoType
  }
}

// Move all below into object Type?

/** A type with a schema-defined name.
 *
 * This includes object types, inferface types and enums.
 */
sealed trait NamedType extends Type {
  /** The name of this type */
  def name: String
  def description: Option[String]
  override def toString: String = name
}

/**
 * A sentinel value indicating the absence of a type.
 */
case object NoType extends Type {
  def describe = "NoType"
}

/**
 * A by name reference to a type defined in `schema`.
 */
case class TypeRef(schema: Schema, ref: String) extends Type {
  override def describe: String = s"@$ref"
}

/**
 * Represents scalar types such as Int, String, and Boolean. Scalars cannot have fields.
 * @see https://facebook.github.io/graphql/draft/#sec-Scalar
 */
case class ScalarType(
  name:        String,
  description: Option[String]
) extends Type with NamedType {
  override def describe: String = name
}

object ScalarType {
  val IntType = ScalarType(
    name = "Int",
    description =
      Some(
        """|The Int scalar type represents a signed 32‐bit numeric non‐fractional value.
           |Response formats that support a 32‐bit integer or a number type should use that
           |type to represent this scalar.
        """.stripMargin.trim
      )
  )
  val FloatType = ScalarType(
    name = "Float",
    description =
      Some(
        """|The Float scalar type represents signed double‐precision fractional values as
           |specified by IEEE 754. Response formats that support an appropriate
           |double‐precision number type should use that type to represent this scalar.
        """.stripMargin.trim
      )
  )
  val StringType = ScalarType(
    name = "String",
    description =
      Some(
        """|The String scalar type represents textual data, represented as UTF‐8 character
           |sequences. The String type is most often used by GraphQL to represent free‐form
           |human‐readable text.
        """.stripMargin.trim
      )
  )
  val BooleanType = ScalarType(
    name = "Boolean",
    description =
      Some(
        """|The Boolean scalar type represents true or false. Response formats should use a
           |built‐in boolean type if supported; otherwise, they should use their
           |representation of the integers 1 and 0.
        """.stripMargin.trim
      )
  )

  val IDType = ScalarType(
    name = "ID",
    description =
      Some(
        """|The ID scalar type represents a unique identifier, often used to refetch an
           |object or as the key for a cache. The ID type is serialized in the same way as a
           |String; however, it is not intended to be human‐readable.
        """.stripMargin.trim
      )
  )
}

/**
 * A type with fields.
 *
 * This includes object types and inferface types.
 */
trait TypeWithFields extends NamedType {
  def fields: List[Field]

  def fieldInfo(name: String): Option[Field] = fields.find(_.name == name)

  override def describe: String = s"$name ${fields.map(_.describe).mkString("{ ", ", ", " }")}"
}

/**
 * Interfaces are an abstract type where there are common fields declared. Any type that
 * implements an interface must define all the fields with names and types exactly matching.
 * @see https://facebook.github.io/graphql/draft/#sec-Interface
 */
case class InterfaceType(
  name:        String,
  description: Option[String],
  fields:      List[Field]
) extends Type with TypeWithFields

/**
 * Object types represent concrete instantiations of sets of fields.
 * @see https://facebook.github.io/graphql/draft/#sec-Object
 */
case class ObjectType(
  name:        String,
  description: Option[String],
  fields:      List[Field],
  interfaces:  List[TypeRef]
) extends Type with TypeWithFields

/**
 * Unions are an abstract type where no common fields are declared. The possible types of a union
 * are explicitly listed out in elements. Types can be made parts of unions without
 * modification of that type.
 * @see https://facebook.github.io/graphql/draft/#sec-Union
 */
case class UnionType(
  name:        String,
  description: Option[String],
  members:     List[TypeRef]
) extends Type with NamedType {
  override def toString: String = members.mkString("|")
  def describe: String = members.map(_.describe).mkString("|")
}

/**
 * Enums are special scalars that can only have a defined set of values.
 * @see https://facebook.github.io/graphql/draft/#sec-Enum
 */
case class EnumType(
  name:        String,
  description: Option[String],
  enumValues:  List[EnumValue]
) extends Type with NamedType {
  def hasValue(name: String): Boolean = enumValues.exists(_.name == name)
  def value(name: String): Option[EnumValue] = enumValues.find(_.name == name)
  def describe: String = s"$name ${enumValues.mkString("{ ", ", ", " }")}"
}

/**
 * The `EnumValue` type represents one of possible values of an enum.
 * @see https://facebook.github.io/graphql/draft/#sec-The-__EnumValue-Type
 */
case class EnumValue(
  name:              String,
  description:       Option[String],
  isDeprecated:      Boolean = false,
  deprecationReason: Option[String] = None
)

/**
 * Input objects are composite types used as inputs into queries defined as a list of named input
 * values.
 * @see https://facebook.github.io/graphql/draft/#sec-Input-Object
 */
case class InputObjectType(
  name:        String,
  description: Option[String],
  inputFields: List[InputValue]
) extends Type with NamedType {
  def inputFieldInfo(name: String): Option[InputValue] = inputFields.find(_.name == name)

  override def describe: String = s"$name ${inputFields.map(_.describe).mkString("{ ", ", ", " }")}"
}

/**
 * Lists represent sequences of values in GraphQL. A List type is a type modifier: it wraps
 * another type instance in the ofType field, which defines the type of each item in the list.
 * @see https://facebook.github.io/graphql/draft/#sec-Type-Kinds.List
 */
case class ListType(
  ofType: Type
) extends Type {
  override def toString: String = s"[$ofType]"
  override def describe: String = s"[${ofType.describe}]"
}

/**
 * A Non‐null type is a type modifier: it wraps another type instance in the `ofType` field.
 * Non‐null types do not allow null as a response, and indicate required inputs for arguments
 * and input object fields.
 * @see https://facebook.github.io/graphql/draft/#sec-Type-Kinds.Non-Null
 */
case class NullableType(
  ofType: Type
) extends Type {
  override def toString: String = s"$ofType?"
  override def describe: String = s"${ofType.describe}?"
}

/**
 * The `Field` type represents each field in an Object or Interface type.
 * @see https://facebook.github.io/graphql/draft/#sec-The-__Field-Type
 */
case class Field private (
  name:              String,
  description:       Option[String],
  args:              List[InputValue],
  tpe:               Type,
  isDeprecated:      Boolean,
  deprecationReason: Option[String]
) {
  def describe: String = s"$name: $tpe"
}

/**
 * @param defaultValue  a String encoding (using the GraphQL language) of the default value used by
 *  this input value in the condition a value is not provided at runtime.
 */
case class InputValue private (
  name:         String,
  description:  Option[String],
  tpe:          Type,
  defaultValue: Option[Value]
) {
  def describe: String = s"$name: $tpe"
}

sealed trait Value
object Value {
  case class IntValue(i: Int) extends Value
  case class FloatValue(f: Double) extends Value
  case class StringValue(s: String) extends Value
  case class BooleanValue(b: Boolean) extends Value
  case class IDValue(id: String) extends Value
  case class UntypedEnumValue(name: String) extends Value
  case class TypedEnumValue(e: EnumValue) extends Value
  case class UntypedVariableValue(name: String) extends Value
  case class ListValue(elems: List[Value]) extends Value
  case class ObjectValue(fields: List[(String, Value)]) extends Value
  case object NullValue extends Value
  case object AbsentValue extends Value

  def checkValue(iv: InputValue, value: Option[Value]): Result[Value] =
    (iv.tpe.dealias, value) match {
      case (_, None) if iv.defaultValue.isDefined =>
        iv.defaultValue.get.rightIor
      case (_: NullableType, None) =>
        AbsentValue.rightIor
      case (_: NullableType, Some(NullValue)) =>
        NullValue.rightIor
      case (NullableType(tpe), Some(_)) =>
        checkValue(iv.copy(tpe = tpe), value)
      case (IntType, Some(value: IntValue)) =>
        value.rightIor
      case (FloatType, Some(value: FloatValue)) =>
        value.rightIor
      case (StringType, Some(value: StringValue)) =>
        value.rightIor
      case (BooleanType, Some(value: BooleanValue)) =>
        value.rightIor
      case (IDType, Some(value: IDValue)) =>
        value.rightIor
      case (IDType, Some(StringValue(s))) =>
        IDValue(s).rightIor
      case (IDType, Some(IntValue(i))) =>
        IDValue(i.toString).rightIor
      case (_: EnumType, Some(value: TypedEnumValue)) =>
        value.rightIor
      case (e: EnumType, Some(UntypedEnumValue(name))) if e.hasValue(name) =>
        TypedEnumValue(e.value(name).get).rightIor
      case (ListType(tpe), Some(ListValue(arr))) =>
        arr.traverse { elem =>
          checkValue(iv.copy(tpe = tpe, defaultValue = None), Some(elem))
        }.map(ListValue)
      case (InputObjectType(_, _, ivs), Some(ObjectValue(fs))) =>
        val obj = fs.toMap
        ivs.traverse(iv => checkValue(iv, obj.get(iv.name)).map(v => (iv.name, v))).map(ObjectValue)
      case (_, Some(value)) => mkErrorResult(s"Expected ${iv.tpe} found '$value' for '${iv.name}")
      case (_, None) => mkErrorResult(s"Value of type ${iv.tpe} required for '${iv.name}")
    }

  def checkVarValue(iv: InputValue, value: Option[Json]): Result[Value] = {
    import io.circe.optics.all._

    (iv.tpe.dealias, value) match {
      case (_, None) if iv.defaultValue.isDefined =>
        iv.defaultValue.get.rightIor
      case (_: NullableType, None) =>
        AbsentValue.rightIor
      case (_: NullableType, Some(jsonNull(_))) =>
        NullValue.rightIor
      case (NullableType(tpe), Some(_)) =>
        checkVarValue(iv.copy(tpe = tpe), value)
      case (IntType, Some(jsonInt(value))) =>
        IntValue(value).rightIor
      case (FloatType, Some(jsonDouble(value))) =>
        FloatValue(value).rightIor
      case (StringType, Some(jsonString(value))) =>
        StringValue(value).rightIor
      case (BooleanType, Some(jsonBoolean(value))) =>
        BooleanValue(value).rightIor
      case (IDType, Some(jsonInt(value))) =>
        IDValue(value.toString).rightIor
      case (IDType, Some(jsonString(value))) =>
        IDValue(value).rightIor
      case (e: EnumType, Some(jsonString(name))) if e.hasValue(name) =>
        TypedEnumValue(e.value(name).get).rightIor
      case (ListType(tpe), Some(jsonArray(arr))) =>
        arr.traverse { elem =>
          checkVarValue(iv.copy(tpe = tpe, defaultValue = None), Some(elem))
        }.map(vs => ListValue(vs.toList))
      case (InputObjectType(_, _, ivs), Some(jsonObject(obj))) =>
        ivs.traverse(iv => checkVarValue(iv, obj(iv.name)).map(v => (iv.name, v))).map(ObjectValue)
      case (_, Some(value)) => mkErrorResult(s"Expected ${iv.tpe} found '$value' for '${iv.name}")
      case (_, None) => mkErrorResult(s"Value of type ${iv.tpe} required for '${iv.name}")
    }
  }
}

/**
 * The `Directive` type represents a Directive that a server supports.
 * @see https://facebook.github.io/graphql/draft/#sec-The-__Directive-Type
 */
case class Directive(
  name:        String,
  description: Option[String],
  locations:   List[DirectiveLocation],
  args:        List[InputValue]
)

sealed trait DirectiveLocation
object DirectiveLocation {
  case object QUERY                  extends DirectiveLocation
  case object MUTATION               extends DirectiveLocation
  case object SUBSCRIPTION           extends DirectiveLocation
  case object FIELD                  extends DirectiveLocation
  case object FRAGMENT_DEFINITION    extends DirectiveLocation
  case object FRAGMENT_SPREAD        extends DirectiveLocation
  case object INLINE_FRAGMENT        extends DirectiveLocation
  case object SCHEMA                 extends DirectiveLocation
  case object SCALAR                 extends DirectiveLocation
  case object OBJECT                 extends DirectiveLocation
  case object FIELD_DEFINITION       extends DirectiveLocation
  case object ARGUMENT_DEFINITION    extends DirectiveLocation
  case object INTERFACE              extends DirectiveLocation
  case object UNION                  extends DirectiveLocation
  case object ENUM                   extends DirectiveLocation
  case object ENUM_VALUE             extends DirectiveLocation
  case object INPUT_OBJECT           extends DirectiveLocation
  case object INPUT_FIELD_DEFINITION extends DirectiveLocation
}
