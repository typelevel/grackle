// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package grackle

object Ast {

  type Document = List[Definition]

  sealed trait Definition

  sealed trait ExecutableDefinition extends Definition
  sealed trait TypeSystemDefinitionOrExtension extends Definition

  sealed trait TypeSystemDefinition extends TypeSystemDefinitionOrExtension
  sealed trait TypeSystemExtension extends TypeSystemDefinitionOrExtension

  sealed trait TypeExtension extends TypeSystemExtension

  sealed abstract class OperationType(val name: String)
  object OperationType {
    case object Query        extends OperationType("query")
    case object Mutation     extends OperationType("mutation")
    case object Subscription extends OperationType("subscription")
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

  case class SchemaExtension(
    rootOperationTypes: List[RootOperationTypeDefinition],
    directives:         List[Directive]
  ) extends TypeSystemExtension

  case class RootOperationTypeDefinition(
    operationType: OperationType,
    tpe:           Type.Named,
    directives: List[Directive]
  )

  sealed trait TypeDefinition extends TypeSystemDefinition with Product with Serializable {
    def name: Name
    def description: Option[String]
    def directives: List[Directive]
  }

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

  case class ScalarTypeExtension(
    name: Name,
    description: Option[String],
    directives: List[Directive]
  ) extends TypeExtension

  case class ObjectTypeExtension(
    name: Name,
    description: Option[String],
    fields: List[FieldDefinition],
    interfaces: List[Type.Named],
    directives: List[Directive]
  ) extends TypeExtension

  case class InterfaceTypeExtension(
    name: Name,
    description: Option[String],
    fields: List[FieldDefinition],
    interfaces: List[Type.Named],
    directives: List[Directive]
  ) extends TypeExtension

  case class UnionTypeExtension(
    name: Name,
    description: Option[String],
    directives: List[Directive],
    members: List[Type.Named]
  ) extends TypeExtension

  case class EnumTypeExtension(
    name: Name,
    description: Option[String],
    directives: List[Directive],
    values: List[EnumValueDefinition]
  ) extends TypeExtension

  case class InputObjectTypeExtension(
    name: Name,
    description: Option[String],
    directives: List[Directive],
    fields: List[InputValueDefinition],
  ) extends TypeExtension

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
