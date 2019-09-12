// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

object Ast {

  sealed trait OperationType
  object OperationType {
    case object Query        extends OperationType
    case object Mutation     extends OperationType
    case object Subscription extends OperationType
  }

  sealed trait OperationDefinition
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
  )

  case class VariableDefinition(
    variable:     Value.Variable,
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

}
