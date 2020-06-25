// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import atto.Atto._
import cats.data.Ior
import cats.implicits._
import io.circe.Json

import QueryInterpreter.{ mkErrorResult, mkOneError }
import ScalarType._, Value._

/**
 * Representation of a GraphQL schema
 *
 * A `Schema` is a collection of type and directive declarations.
 */
trait Schema {
  /** The types defined by this `Schema`. */
  def types: List[NamedType]
  /** The directives defined by this `Schema`. */
  def directives: List[Directive]

  /** A reference by name to a type defined by this `Schema`.
   *
   *  `TypeRef`s refer to types defined in this schema by name and hence
   *  can be used as part of mutually recursive type definitions.
   */
  def ref(tpnme: String): TypeRef = new TypeRef(this, tpnme)

  /**
   * Alias for `ref` for use within constructors of concrete
   * `Schema` values.
   */
  protected def TypeRef(tpnme: String): TypeRef = ref(tpnme)

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
  def defaultSchemaType: NamedType = {
    def mkRootDef(fieldName: String)(tpe: NamedType): Field =
      Field(fieldName, None, Nil, tpe, false, None)

    ObjectType(
      name = "Schema",
      description = None,
      fields =
        List(
          definition("Query").map(mkRootDef("query")),
          definition("Mutation").map(mkRootDef("mutation")),
          definition("Subscription").map(mkRootDef("subscription"))
        ).flatten,
      interfaces = Nil
    )
  }

  /**
   * Look up by name a type defined in this `Schema`.
   *
   * Yields the type, if defined, `NoType` otherwise.
   */
  def definition(name: String): Option[NamedType] =
    types.find(_.name == name).orElse(ScalarType(name)).map(_.dealias)

  def ref(tp: Type): Option[TypeRef] = tp match {
    case nt: NamedType if types.exists(_.name == nt.name) => Some(ref(nt.name))
    case _ => None
  }

  /**
   * The schema type.
   *
   * Either the explicitly defined type named `"Schema"` or the default
   * schema type if not defined.
   */
  def schemaType: NamedType = definition("Schema").getOrElse(defaultSchemaType)

  /** The type of queries defined by this `Schema` */
  def queryType: NamedType = schemaType.field("query").asNamed.get
  /** The type of mutations defined by this `Schema` */
  def mutationType: Option[NamedType] = schemaType.field("mutation").asNamed
  /** The type of subscriptions defined by this `Schema` */
  def subscriptionType: Option[NamedType] = schemaType.field("subscription").asNamed

  override def toString = SchemaRenderer.renderSchema(this)
}

object Schema {
  def apply(schemaText: String): Result[Schema] =
    SchemaParser.parseText(schemaText)
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

  /** `true` if this type is a subtype of `other`. */
  def <:<(other: Type): Boolean =
    (this.dealias, other.dealias) match {
      case (tp1, tp2) if tp1 == tp2               => true
      case (tp1, UnionType(_, _, members))        => members.exists(tp1 <:< _.dealias)
      case (ObjectType(_, _, _, interfaces), tp2) => interfaces.exists(_.dealias == tp2)
      case _ => false
    }

  def nominal_=:=(other: Type): Boolean =
    this =:= other ||
      ((this.dealias, other.dealias) match {
        case (nt1: NamedType, nt2: NamedType) => nt1.name == nt2.name
        case _ => false
      })

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
    case TypeRef(_, _) if exists => dealias.field(fieldName)
    case ObjectType(_, _, fields, _) => fields.find(_.name == fieldName).map(_.tpe).getOrElse(NoType)
    case InterfaceType(_, _, fields, _) => fields.find(_.name == fieldName).map(_.tpe).getOrElse(NoType)
    case _ => NoType
  }

  /** `true` if this type has a field named `fieldName`, false otherwise. */
  def hasField(fieldName: String): Boolean =
    field(fieldName) != NoType

  def withField[T](fieldName: String)(body: Type => Result[T]): Result[T] = {
    val childTpe = field(fieldName)
    if (childTpe =:= NoType) mkErrorResult(s"Unknown field '$fieldName' in '$this'")
    else body(childTpe)
  }

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
    case (fieldName :: rest, InterfaceType(_, _, fields, _)) =>
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
    case (fieldName :: rest, InterfaceType(_, _, fields, _)) =>
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
    case (fieldName :: rest, InterfaceType(_, _, fields, _)) =>
      fields.find(_.name == fieldName).map(_.tpe.pathIsNullable(rest)).getOrElse(false)
    case _ => false
  }

  /** Strip off aliases */
  def dealias: Type = this

  /** true if a non-TypeRef or a TypeRef to a defined type */
  def exists: Boolean = true

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

  /** This type if it is a (nullable) list, `ListType(this)` otherwise. */
  def list: Type = this match {
    case l: ListType => l
    case NullableType(tpe) => NullableType(tpe.list)
    case tpe => ListType(tpe)
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
    case InterfaceType(_, _, fields, _) => fields.find(_.name == fieldName).map(_.tpe).getOrElse(NoType)
    case _ => NoType
  }

  def withUnderlyingField[T](fieldName: String)(body: Type => Result[T]): Result[T] =
    underlyingObject.withField(fieldName)(body)

  /** Is this type a leaf type?
   *
   * `true` if after stripping of aliases the underlying type a scalar or an
   * enum, `false` otherwise.
   */
  def isLeaf: Boolean = this match {
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
    case TypeRef(_, _) => dealias.asLeaf
    case _: ScalarType => this
    case _: EnumType => this
    case _ => NoType
  }

  def isNamed: Boolean = false

  def asNamed: Option[NamedType] = None
}

// Move all below into object Type?

/** A type with a schema-defined name.
 *
 * This includes object types, inferface types and enums.
 */
sealed trait NamedType extends Type {
  /** The name of this type */
  def name: String
  override def dealias: NamedType = this
  override def isNamed: Boolean = true
  override def asNamed: Option[NamedType] = Some(this)
  def description: Option[String]
  override def toString: String = name
}

/**
 * A sentinel value indicating the absence of a type.
 */
case object NoType extends Type

/**
 * A synthetic type representing the join between a component/stage and its
 * parent object.
 */
object JoinType {
  def apply(fieldName: String, tpe: Type): ObjectType =
    ObjectType(s"<$fieldName:$tpe>", None, List(Field(fieldName, None, Nil, tpe, false, None)), Nil)

  def unapply(ot: ObjectType): Option[(String, Type)] = ot match {
    case ObjectType(nme, None, List(Field(fieldName, None, Nil, tpe, false, None)), Nil) if nme == s"<$fieldName:$tpe>" =>
      Some((fieldName, tpe))
  }
}
/**
 * A by name reference to a type defined in `schema`.
 */
case class TypeRef(schema: Schema, name: String) extends NamedType {
  override def dealias: NamedType = schema.definition(name).getOrElse(this)
  override def exists: Boolean = schema.definition(name).isDefined
  def description: Option[String] = dealias.description
}

/**
 * Represents scalar types such as Int, String, and Boolean. Scalars cannot have fields.
 * @see https://facebook.github.io/graphql/draft/#sec-Scalar
 */
case class ScalarType(
  name:        String,
  description: Option[String]
) extends Type with NamedType

object ScalarType {
  def apply(tpnme: String): Option[ScalarType] = tpnme match {
    case "Int" => Some(IntType)
    case "Float" => Some(FloatType)
    case "String" => Some(StringType)
    case "Boolean" => Some(BooleanType)
    case "ID" => Some(IDType)
    case _ => None
  }

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
sealed trait TypeWithFields extends NamedType {
  def fields: List[Field]

  def fieldInfo(name: String): Option[Field] = fields.find(_.name == name)
}

/**
 * Interfaces are an abstract type where there are common fields declared. Any type that
 * implements an interface must define all the fields with names and types exactly matching.
 * @see https://facebook.github.io/graphql/draft/#sec-Interface
 */
case class InterfaceType(
  name:        String,
  description: Option[String],
  fields:      List[Field],
  interfaces:  List[NamedType]
) extends Type with TypeWithFields

/**
 * Object types represent concrete instantiations of sets of fields.
 * @see https://facebook.github.io/graphql/draft/#sec-Object
 */
case class ObjectType(
  name:        String,
  description: Option[String],
  fields:      List[Field],
  interfaces:  List[NamedType]
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
  members:     List[NamedType]
) extends Type with NamedType {
  override def toString: String = members.mkString("|")
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
)

/**
 * @param defaultValue  a String encoding (using the GraphQL language) of the default value used by
 *  this input value in the condition a value is not provided at runtime.
 */
case class InputValue private (
  name:         String,
  description:  Option[String],
  tpe:          Type,
  defaultValue: Option[Value]
)

sealed trait Value
object Value {
  case class IntValue(value: Int) extends Value
  case class FloatValue(value: Double) extends Value
  case class StringValue(value: String) extends Value
  case class BooleanValue(value: Boolean) extends Value
  case class IDValue(value: String) extends Value
  case class UntypedEnumValue(name: String) extends Value
  case class TypedEnumValue(value: EnumValue) extends Value
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
      case (_: ScalarType, Some(value)) => value.rightIor
      case (tpe, Some(value)) => mkErrorResult(s"Expected $tpe found '$value' for '${iv.name}'")
      case (tpe, None) => mkErrorResult(s"Value of type $tpe required for '${iv.name}'")
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
      //case (ScalarType, Some(value)) => value.rightIor
      case (tpe, Some(value)) => mkErrorResult(s"Expected $tpe found '$value' for '${iv.name}'")
      case (tpe, None) => mkErrorResult(s"Value of type $tpe required for '${iv.name}'")
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
  locations:   List[Ast.DirectiveLocation],
  args:        List[InputValue]
)

/**
 * GraphQL schema parser
 */
object SchemaParser {
  import Ast.{ Type => _, Value => _, Directive => DefinedDirective, _ }, OperationType._

  /**
   *  Parse a query String to a query algebra term.
   *
   *  Yields a Query value on the right and accumulates errors on the left.
   */
  def parseText(text: String): Result[Schema] = {
    def toResult[T](pr: Either[String, T]): Result[T] =
      Ior.fromEither(pr).leftMap(mkOneError(_))

    for {
      doc   <- toResult(GraphQLParser.Document.parseOnly(text).either)
      query <- parseDocument(doc)
    } yield query
  }

  def parseDocument(doc: Document): Result[Schema] = {
    def mkSchemaType(schema: Schema): Result[NamedType] = {
      def mkRootOperationType(rootTpe: RootOperationTypeDefinition): Result[(OperationType, NamedType)] = {
        val RootOperationTypeDefinition(optype, tpe) = rootTpe
        mkType(schema)(tpe).flatMap {
          case nt: NamedType => (optype, nt).rightIor
          case _ => mkErrorResult("Root operation types must be named types")
        }
      }

      def build(query: NamedType, mutation: Option[NamedType], subscription: Option[NamedType]): NamedType = {
        def mkRootDef(fieldName: String)(tpe: NamedType): Field =
          Field(fieldName, None, Nil, tpe, false, None)

        ObjectType(
          name = "Schema",
          description = None,
          fields =
            mkRootDef("query")(query) ::
            List(
              mutation.map(mkRootDef("mutation")),
              subscription.map(mkRootDef("subscription"))
            ).flatten,
          interfaces = Nil
        )
      }

      def defaultQueryType = schema.ref("Query")

      val defns = doc.collect { case schema: SchemaDefinition => schema }
      defns match {
        case Nil => build(defaultQueryType, None, None).rightIor
        case SchemaDefinition(rootTpes, _) :: Nil =>
          rootTpes.traverse(mkRootOperationType).map { ops0 =>
            val ops = ops0.toMap
            build(ops.get(Query).getOrElse(defaultQueryType), ops.get(Mutation), ops.get(Subscription))
          }

        case _ => mkErrorResult("At most one schema definition permitted")
      }
    }

    def mkTypeDefs(schema: Schema): Result[List[NamedType]] = {
      val defns = doc.collect { case tpe: TypeDefinition => tpe }
      defns.traverse(mkTypeDef(schema))
    }

    def mkTypeDef(schema: Schema)(td: TypeDefinition): Result[NamedType] = td match {
      case ScalarTypeDefinition(Name("Int"), _, _) => IntType.rightIor
      case ScalarTypeDefinition(Name("Float"), _, _) => FloatType.rightIor
      case ScalarTypeDefinition(Name("String"), _, _) => StringType.rightIor
      case ScalarTypeDefinition(Name("Boolean"), _, _) => BooleanType.rightIor
      case ScalarTypeDefinition(Name("ID"), _, _) => IDType.rightIor
      case ScalarTypeDefinition(Name(nme), desc, _) => ScalarType(nme, desc).rightIor
      case ObjectTypeDefinition(Name(nme), desc, fields0, ifs0, _) =>
        for {
          fields <- fields0.traverse(mkField(schema))
          ifs    =  ifs0.map { case Ast.Type.Named(Name(nme)) => schema.ref(nme) }
        } yield ObjectType(nme, desc, fields, ifs)
      case InterfaceTypeDefinition(Name(nme), desc, fields0, ifs0, _) =>
        for {
          fields <- fields0.traverse(mkField(schema))
          ifs    =  ifs0.map { case Ast.Type.Named(Name(nme)) => schema.ref(nme) }
        } yield InterfaceType(nme, desc, fields, ifs)
      case UnionTypeDefinition(Name(nme), desc, _, members0) =>
        val members = members0.map { case Ast.Type.Named(Name(nme)) => schema.ref(nme) }
        UnionType(nme, desc, members).rightIor
      case EnumTypeDefinition(Name(nme), desc, _, values0) =>
        for {
          values <- values0.traverse(mkEnumValue)
        } yield EnumType(nme, desc, values)
      case InputObjectTypeDefinition(Name(nme), desc, fields0, _) =>
        for {
          fields <- fields0.traverse(mkInputValue(schema))
        } yield InputObjectType(nme, desc, fields)
    }

    def mkField(schema: Schema)(f: FieldDefinition): Result[Field] = {
      val FieldDefinition(Name(nme), desc, args0, tpe0, dirs) = f
      for {
        args <- args0.traverse(mkInputValue(schema))
        tpe  <- mkType(schema)(tpe0)
        deprecation <- parseDeprecated(dirs)
        (isDeprecated, reason) = deprecation
      } yield Field(nme, desc, args, tpe, isDeprecated, reason)
    }

    def mkType(schema: Schema)(tpe: Ast.Type): Result[Type] = {
      def loop(tpe: Ast.Type, nullable: Boolean): Result[Type] = {
        def wrap(tpe: Type): Type = if (nullable) NullableType(tpe) else tpe
        tpe match {
          case Ast.Type.List(tpe) => loop(tpe, true).map(tpe => wrap(ListType(tpe)))
          case Ast.Type.NonNull(Left(tpe)) => loop(tpe, false)
          case Ast.Type.NonNull(Right(tpe)) => loop(tpe, false)
          case Ast.Type.Named(Name(nme)) => wrap(ScalarType(nme).getOrElse(schema.ref(nme))).rightIor
        }
      }
      loop(tpe, true)
    }

    def mkInputValue(schema: Schema)(f: InputValueDefinition): Result[InputValue] = {
      val InputValueDefinition(Name(nme), desc, tpe0, default0, _) = f
      for {
        tpe  <- mkType(schema)(tpe0)
        dflt <- default0.traverse(parseValue)
      } yield InputValue(nme, desc, tpe, dflt)
    }

    def mkEnumValue(e: EnumValueDefinition): Result[EnumValue] = {
      val EnumValueDefinition(Name(nme), desc, dirs) = e
      for {
        deprecation <- parseDeprecated(dirs)
        (isDeprecated, reason) = deprecation
      } yield EnumValue(nme, desc, isDeprecated, reason)
    }

    def parseDeprecated(directives: List[DefinedDirective]): Result[(Boolean, Option[String])] =
      directives.collect { case dir@DefinedDirective(Name("deprecated"), _) => dir } match {
        case Nil => (false, None).rightIor
        case DefinedDirective(_, List((Name("reason"), Ast.Value.StringValue(reason)))) :: Nil => (true, Some(reason)).rightIor
        case DefinedDirective(_, Nil) :: Nil => (true, Some("No longer supported")).rightIor
        case DefinedDirective(_, _) :: Nil => mkErrorResult(s"deprecated must have a single String 'reason' argument, or no arguments")
        case _ => mkErrorResult(s"Only a single deprecated allowed at a given location")
      }

    // Share with Query parser
    def parseValue(value: Ast.Value): Result[Value] = {
      value match {
        case Ast.Value.IntValue(i) => IntValue(i).rightIor
        case Ast.Value.FloatValue(d) => FloatValue(d).rightIor
        case Ast.Value.StringValue(s) => StringValue(s).rightIor
        case Ast.Value.BooleanValue(b) => BooleanValue(b).rightIor
        case Ast.Value.EnumValue(e) => UntypedEnumValue(e.value).rightIor
        case Ast.Value.Variable(v) => UntypedVariableValue(v.value).rightIor
        case Ast.Value.NullValue => NullValue.rightIor
        case Ast.Value.ListValue(vs) => vs.traverse(parseValue).map(ListValue)
        case Ast.Value.ObjectValue(fs) =>
          fs.traverse { case (name, value) =>
            parseValue(value).map(v => (name.value, v))
          }.map(ObjectValue)
      }
    }

    object schema extends Schema {
      var types: List[NamedType] = Nil
      var schemaType1: NamedType = null
      override def schemaType: NamedType = schemaType1
      var directives: List[Directive] = Nil

      def complete(types0: List[NamedType], schemaType0: NamedType, directives0: List[Directive]): this.type = {
        types = types0
        schemaType1 = schemaType0
        directives = directives0
        this
      }
    }

    for {
      types      <- mkTypeDefs(schema)
      schemaType <- mkSchemaType(schema)
    } yield schema.complete(types, schemaType, Nil)
  }
}

object SchemaRenderer {
  def renderSchema(schema: Schema): String = {
    def mkRootDef(fieldName: String)(tpe: NamedType): String =
      s"$fieldName: ${tpe.name}"

    val fields =
      mkRootDef("query")(schema.queryType) ::
      List(
        schema.mutationType.map(mkRootDef("mutation")),
        schema.subscriptionType.map(mkRootDef("subscription"))
      ).flatten

    val schemaDefn =
      if (fields.size == 1 && schema.queryType =:= schema.ref("Query")) ""
      else fields.mkString("schema {\n  ", "\n  ", "\n}\n")

    schemaDefn ++
    schema.types.map(renderTypeDefn).mkString("\n")
  }

  def renderTypeDefn(tpe: NamedType): String = {
    def renderField(f: Field): String = {
      val Field(nme, _, args, tpe, isDeprecated, reason) = f
      val dep = renderDeprecation(isDeprecated, reason)
      if (args.isEmpty)
        s"$nme: ${renderType(tpe)}" + dep
      else
        s"$nme(${args.map(renderInputValue).mkString(", ")}): ${renderType(tpe)}" + dep
    }

    tpe match {
      case tr: TypeRef => renderTypeDefn(tr.dealias)

      case ScalarType(nme, _) =>
        s"""scalar $nme"""

      case ObjectType(nme, _, fields, ifs0) =>
        val ifs = if(ifs0.isEmpty) "" else " implements "+ifs0.map(_.name).mkString("&")

        s"""|type $nme$ifs {
            |  ${fields.map(renderField).mkString("\n  ")}
            |}""".stripMargin

      case InterfaceType(nme, _, fields, ifs0) =>
        val ifs = if(ifs0.isEmpty) "" else " implements "+ifs0.map(_.name).mkString("&")

        s"""|interface $nme$ifs {
            |  ${fields.map(renderField).mkString("\n  ")}
            |}""".stripMargin

      case UnionType(nme, _, members) =>
        s"""union $nme = ${members.map(_.name).mkString(" | ")}"""

      case EnumType(nme, _, values) =>
        s"""|enum $nme {
            |  ${values.map(renderEnumValue).mkString("\n  ")}
            |}""".stripMargin

      case InputObjectType(nme, _, fields) =>
        s"""|input $nme {
            |  ${fields.map(renderInputValue).mkString("\n  ")}
            |}""".stripMargin
    }
  }

  def renderType(tpe: Type): String = {
    def loop(tpe: Type, nullable: Boolean): String = {
      def wrap(tpe: String) = if (nullable) tpe else s"$tpe!"

      tpe match {
        case NullableType(tpe) => loop(tpe, true)
        case ListType(tpe)     => wrap(s"[${loop(tpe, false)}]")
        case nt: NamedType     => wrap(nt.name)
        case NoType            => "NoType"
      }
    }

    loop(tpe, false)
  }

  def renderEnumValue(v: EnumValue): String = {
    val EnumValue(nme, _, isDeprecated, reason) = v
    s"$nme" + renderDeprecation(isDeprecated, reason)
  }

  def renderInputValue(iv: InputValue): String = {
    val InputValue(nme, _, tpe, default) = iv
    val df = default.map(v => s" = ${renderValue(v)}").getOrElse("")
    s"$nme: ${renderType(tpe)}$df"
  }

  def renderValue(value: Value): String = value match {
    case IntValue(i) => i.toString
    case FloatValue(f) => f.toString
    case StringValue(s) => s""""$s""""
    case BooleanValue(b) => b.toString
    case IDValue(i) => s""""$i""""
    case TypedEnumValue(e) => e.name
    case ListValue(elems) => elems.map(renderValue).mkString("[", ", ", "]")
    case ObjectValue(fields) =>
        fields.map {
          case (name, value) => s"$name : ${renderValue(value)}"
        }.mkString("{", ", ", "}")
    case _ => "null"
  }

  def renderDeprecation(isDeprecated: Boolean, reason: Option[String]): String =
    if (isDeprecated) " @deprecated" + reason.fold("")(r => "(reason: \"" + r + "\")") else ""
}
