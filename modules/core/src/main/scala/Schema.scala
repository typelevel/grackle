// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

object Schema {

  case class Schema(
    queryType:        ObjectType,
    mutationType:     Option[ObjectType],
    subscriptionType: Option[ObjectType],
    directives:       List[Directive]
  )

  sealed trait Type

  /** Marker trait for types that can be wrapped with NonNull. */
  sealed trait NullableType extends Type

  /**
   * Represents scalar types such as Int, String, and Boolean. Scalars cannot have fields.
   * @see https://facebook.github.io/graphql/draft/#sec-Scalar
   */
  case class ScalarType(
    name:        String,
    description: Option[String]
  ) extends NullableType

  object ScalarType {
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
  ) extends NullableType

  /**
   * Object types represent concrete instantiations of sets of fields.
   * @see https://facebook.github.io/graphql/draft/#sec-Object
   */
  case class ObjectType(
    name:        String,
    description: Option[String],
    fields:      List[Field],
    interfaces:  List[InterfaceType]
  ) extends NullableType

  /**
   * Unions are an abstract type where no common fields are declared. The possible types of a union
   * are explicitly listed out in elements. Types can be made parts of unions without
   * modification of that type.
   * @see https://facebook.github.io/graphql/draft/#sec-Union
   */
  case class UnionType(
    name:        String,
    description: Option[String],
    members:     List[ObjectType]
  ) extends NullableType

  /**
   * Enums are special scalars that can only have a defined set of values.
   * @see https://facebook.github.io/graphql/draft/#sec-Enum
   */
  case class EnumType(
    name:        String,
    description: Option[String],
    enumValues:  List[EnumValue]
  ) extends NullableType

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
  ) extends NullableType

  /**
   * A Non‐null type is a type modifier: it wraps another type instance in the `ofType` field.
   * Non‐null types do not allow null as a response, and indicate required inputs for arguments
   * and input object fields.
   * @see https://facebook.github.io/graphql/draft/#sec-Type-Kinds.Non-Null
   */
  case class NonNullType(
    ofType: NullableType
  ) extends Type

  /**
   * The `Field` type represents each field in an Object or Interface type.
   * @see https://facebook.github.io/graphql/draft/#sec-The-__Field-Type
   */
  case class Field private (
    name:              String,
    description:       Option[String],
    args:              List[InputValue],
    isDeprecated:      Boolean,
    deprecationReason: Option[String]
  )(
    tpe0:              => Type
  ) {
    lazy val tpe = tpe0
  }
  object Field {
    def apply(
      name:              String,
      description:       Option[String],
      args:              List[InputValue],
      tpe:               => Type,
      isDeprecated:      Boolean,
      deprecationReason: Option[String]
    ) = new Field(name, description, args, isDeprecated, deprecationReason)(tpe)
  }

  /**
   * @param defaultValue  a String encoding (using the GraphQL language) of the default value used by
   *  this input value in the condition a value is not provided at runtime.
   */
  case class InputValue private (
    name:         String,
    description:  Option[String],
    defaultValue: Option[String]
  )(
    tpe0:         => Type
  ) {
    lazy val tpe = tpe0
  }
  object InputValue {
    def apply(
      name:              String,
      description:       Option[String],
      tpe:               => Type,
      defaultValue:      Option[String]
    ) = new InputValue(name, description, defaultValue)(tpe)
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
}
