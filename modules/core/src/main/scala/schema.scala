// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

trait Schema {
  val types:            List[NamedType]
  val queryType:        TypeRef
  val mutationType:     Option[TypeRef]
  val subscriptionType: Option[TypeRef]
  val directives:       List[Directive]

  def TypeRef(ref: String): TypeRef =
    new TypeRef(this, ref)
}

sealed trait Type {
  def field(fieldName: String): Type = this match {
    case NullableType(tpe) => tpe.field(fieldName)
    case TypeRef(_, _) => dealias.field(fieldName)
    case ObjectType(_, _, fields, _) => fields.find(_.name == fieldName).map(_.tpe).getOrElse(NoType)
    case InterfaceType(_, _, fields) => fields.find(_.name == fieldName).map(_.tpe).getOrElse(NoType)
    case _ => NoType
  }

  def =:=(other: Type): Boolean = (this eq other) || (dealias == other.dealias)

  def dealias: Type = this match {
    case TypeRef(schema, tpnme) => schema.types.find(_.name == tpnme).getOrElse(NoType)
    case _ => this
  }

  def isNullable: Boolean = this match {
    case NullableType(_) => true
    case _ => false
  }

  def nonNull: Type = this match {
    case NullableType(tpe) => tpe.nonNull
    case _ => this
  }

  def isList: Boolean = this match {
    case ListType(_) => true
    case _ => false
  }

  def item: Type = this match {
    case NullableType(tpe) => tpe.item
    case ListType(tpe) => tpe
    case _ => NoType
  }

  def underlyingObject: Type = this match {
    case NullableType(tpe) => tpe.underlyingObject
    case ListType(tpe) => tpe.underlyingObject
    case _: TypeRef => dealias.underlyingObject
    case o: ObjectType => o
    case i: InterfaceType => i
    case _ => NoType
  }

  def underlyingField(fieldName: String): Type = this match {
    case NullableType(tpe) => tpe.underlyingField(fieldName)
    case ListType(tpe) => tpe.underlyingField(fieldName)
    case TypeRef(_, _) => dealias.underlyingField(fieldName)
    case ObjectType(_, _, fields, _) => fields.find(_.name == fieldName).map(_.tpe).getOrElse(NoType)
    case InterfaceType(_, _, fields) => fields.find(_.name == fieldName).map(_.tpe).getOrElse(NoType)
    case _ => NoType
  }

  def shortString: String = toString

  def isLeaf: Boolean = this match {
    case NullableType(tpe) => tpe.isLeaf
    case _: ScalarType => true
    case _: EnumType => true
    case _ => false
  }
}

sealed trait NamedType extends Type {
  def name: String
  override def shortString: String = name
}

case object NoType extends Type
case class TypeRef(schema: Schema, ref: String) extends Type {
  override def toString: String = s"@$ref"
}

/**
 * Represents scalar types such as Int, String, and Boolean. Scalars cannot have fields.
 * @see https://facebook.github.io/graphql/draft/#sec-Scalar
 */
case class ScalarType(
  name:        String,
  description: Option[String]
) extends Type {
  override def toString: String = name
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
) extends Type with NamedType

/**
 * Object types represent concrete instantiations of sets of fields.
 * @see https://facebook.github.io/graphql/draft/#sec-Object
 */
case class ObjectType(
  name:        String,
  description: Option[String],
  fields:      List[Field],
  interfaces:  List[TypeRef]
) extends Type with NamedType {
  override def toString: String = s"$name ${fields.mkString("{", ", ", "}")}"
}

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
) extends Type

/**
 * Enums are special scalars that can only have a defined set of values.
 * @see https://facebook.github.io/graphql/draft/#sec-Enum
 */
case class EnumType(
  name:        String,
  description: Option[String],
  enumValues:  List[EnumValue]
) extends Type with NamedType

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
)

/**
 * Lists represent sequences of values in GraphQL. A List type is a type modifier: it wraps
 * another type instance in the ofType field, which defines the type of each item in the list.
 * @see https://facebook.github.io/graphql/draft/#sec-Type-Kinds.List
 */
case class ListType(
  ofType: Type
) extends Type {
  override def shortString: String = s"[${ofType.shortString}]"
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
  override def shortString: String = s"${ofType.shortString}?"
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
) {
  override def toString: String = s"$name: $tpe"
}

/**
 * @param defaultValue  a String encoding (using the GraphQL language) of the default value used by
 *  this input value in the condition a value is not provided at runtime.
 */
case class InputValue private (
  name:         String,
  description:  Option[String],
  tpe:          Type,
  defaultValue: Option[String]
)

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
