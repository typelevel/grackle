// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

object Ast {

  type Document = List[Definition]

  sealed trait Definition

  sealed trait ExecutableDefinition extends Definition
  sealed trait TypeSystemDefinition extends Definition

  sealed trait OperationType
  object OperationType {
    case object Query        extends OperationType
    case object Mutation     extends OperationType
    case object Subscription extends OperationType
  }

  sealed trait OperationDefinition extends ExecutableDefinition
  object OperationDefinition {

    case class QueryShorthand(
      selectionSet: List[Selection]
    ) extends OperationDefinition

    case class Operation(
      operationType: OperationType,
      name:          Option[Name],
      variables:     List[VariableDefinition],
      directives:    List[Directive],
      selectionSet:  List[Selection]
    ) extends OperationDefinition

  }

  case class Name(value: String)

  case class Directive(name: Name, arguments: List[(Name, Value)])

  sealed trait Selection
  object Selection {

    case class Field(
      alias:        Option[Name],
      name:         Name,
      arguments:    List[(Name, Value)],
      directives:   List[Directive],
      selectionSet: List[Selection]
    ) extends Selection

    case class FragmentSpread(
      name: Name,
      directives: List[Directive]
    ) extends Selection

    case class InlineFragment(
      typeCondition: Option[Type],
      directives:    List[Directive],
      selectionSet:  List[Selection]
    ) extends Selection

  }

  case class FragmentDefinition(
    name:          Name,
    typeCondition: Type,
    directives:    List[Directive],
    selectionSet:  List[Selection]
  ) extends ExecutableDefinition

  case class VariableDefinition(
    name:         Name,
    tpe:          Type,
    defaultValue: Option[Value],
    directives:   List[Directive]
  )

  sealed trait Value
  object Value {
    case class  Variable(name: Name)           extends Value
    case class  IntValue(value: Int)           extends Value
    case class  FloatValue(value: Double)      extends Value
    case class  StringValue(value: String)     extends Value
    case class  BooleanValue(value: Boolean)   extends Value
    case object NullValue                      extends Value
    case class  EnumValue(name: Name)          extends Value
    case class  ListValue(values: List[Value]) extends Value
    case class  ObjectValue(fields: List[(Name, Value)]) extends Value
  }

  sealed abstract class Type(val name: String)
  object Type {
    case class Named(astName: Name)             extends Type(astName.value)
    case class List(ofType: Type)               extends Type(s"[${ofType.name}]")
    case class NonNull(of: Either[Named, List]) extends Type(s"${of.merge.name}!")
  }

  case class SchemaDefinition(
    rootOperationTypes: List[RootOperationTypeDefinition],
    directives:         List[Directive]
  ) extends TypeSystemDefinition

  case class RootOperationTypeDefinition(
    operationType: OperationType,
    tpe:           Type.Named
  )

  sealed trait TypeDefinition extends TypeSystemDefinition

  case class ScalarTypeDefinition(
    name: Name,
    description: Option[String],
    directives: List[Directive]
  ) extends TypeDefinition

  case class ObjectTypeDefinition(
    name: Name,
    description: Option[String],
    fields: List[FieldDefinition],
    interfaces: List[Type.Named],
    directives: List[Directive]
  ) extends TypeDefinition

  case class InterfaceTypeDefinition(
    name: Name,
    description: Option[String],
    fields: List[FieldDefinition],
    interfaces: List[Type.Named],
    directives: List[Directive]
  ) extends TypeDefinition

  case class UnionTypeDefinition(
    name: Name,
    description: Option[String],
    directives: List[Directive],
    members: List[Type.Named]
  ) extends TypeDefinition

  case class EnumTypeDefinition(
    name: Name,
    description: Option[String],
    directives: List[Directive],
    values: List[EnumValueDefinition]
  ) extends TypeDefinition

  case class FieldDefinition(
    name: Name,
    description: Option[String],
    args: List[InputValueDefinition],
    tpe: Type,
    directives: List[Directive]
  )

  case class EnumValueDefinition(
    name: Name,
    description: Option[String],
    directives: List[Directive],
  )

  case class InputValueDefinition(
    name: Name,
    description: Option[String],
    tpe: Type,
    defaultValue: Option[Value],
    directives: List[Directive]
  )

  case class InputObjectTypeDefinition(
    name: Name,
    description: Option[String],
    fields: List[InputValueDefinition],
    directives: List[Directive]
  ) extends TypeDefinition

  case class DirectiveDefinition(
    name: Name,
    description: Option[String],
    args: List[InputValueDefinition],
    repeatable: Boolean,
    locations: List[DirectiveLocation]
  ) extends TypeSystemDefinition

  sealed trait DirectiveLocation
  object DirectiveLocation {
    case object QUERY                  extends DirectiveLocation
    case object MUTATION               extends DirectiveLocation
    case object SUBSCRIPTION           extends DirectiveLocation
    case object FIELD                  extends DirectiveLocation
    case object FRAGMENT_DEFINITION    extends DirectiveLocation
    case object FRAGMENT_SPREAD        extends DirectiveLocation
    case object INLINE_FRAGMENT        extends DirectiveLocation
    case object VARIABLE_DEFINITION    extends DirectiveLocation

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
