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

import io.circe.Encoder

import ScalarType._

object Introspection {
  val schema =
    // N.B. can't use schema"..." here because it's a macro defined in the same module
    Schema(
      """
        type Query {
          __schema: __Schema!
          __type(name: String!): __Type
        }

        type __Schema {
          types: [__Type!]!
          queryType: __Type!
          mutationType: __Type
          subscriptionType: __Type
          directives: [__Directive!]!
        }

        type __Type {
          kind: __TypeKind!
          name: String
          description: String

          # may be non-null for custom SCALAR, otherwise null.
          specifiedByURL: String

          # must be non-null for OBJECT and INTERFACE, otherwise null.
          fields(includeDeprecated: Boolean! = false): [__Field!]

          # must be non-null for OBJECT and INTERFACE, otherwise null.
          interfaces: [__Type!]

          # must be non-null for INTERFACE and UNION, otherwise null.
          possibleTypes: [__Type!]

          # must be non-null for ENUM, otherwise null.
          enumValues(includeDeprecated: Boolean! = false): [__EnumValue!]

          # must be non-null for INPUT_OBJECT, otherwise null.
          inputFields(includeDeprecated: Boolean! = false): [__InputValue!]

          # must be non-null for NON_NULL and LIST, otherwise null.
          ofType: __Type

          # must be non-null for INPUT_OBJECT, otherwise null.
          isOneOf: Boolean
        }

        type __Field {
          name: String!
          description: String
          args(includeDeprecated: Boolean! = false): [__InputValue!]!
          type: __Type!
          isDeprecated: Boolean!
          deprecationReason: String
        }

        type __InputValue {
          name: String!
          description: String
          type: __Type!
          defaultValue: String
          isDeprecated: Boolean!
          deprecationReason: String
        }

        type __EnumValue {
          name: String!
          description: String
          isDeprecated: Boolean!
          deprecationReason: String
        }

        enum __TypeKind {
          SCALAR
          OBJECT
          INTERFACE
          UNION
          ENUM
          INPUT_OBJECT
          LIST
          NON_NULL
        }

        type __Directive {
          name: String!
          description: String
          locations: [__DirectiveLocation!]!
          args(includeDeprecated: Boolean! = false): [__InputValue!]!
          isRepeatable: Boolean!
        }

        enum __DirectiveLocation {
          QUERY
          MUTATION
          SUBSCRIPTION
          FIELD
          FRAGMENT_DEFINITION
          FRAGMENT_SPREAD
          INLINE_FRAGMENT
          VARIABLE_DEFINITION
          SCHEMA
          SCALAR
          OBJECT
          FIELD_DEFINITION
          ARGUMENT_DEFINITION
          INTERFACE
          UNION
          ENUM
          ENUM_VALUE
          INPUT_OBJECT
          INPUT_FIELD_DEFINITION
        }
      """
    ).toOption.get

  val QueryType = schema.uncheckedRef(schema.queryType)
  val __SchemaType = schema.ref("__Schema")
  val __TypeType =  schema.ref("__Type")
  val __FieldType =  schema.ref("__Field")
  val __InputValueType =  schema.ref("__InputValue")
  val __EnumValueType =  schema.ref("__EnumValue")
  val __DirectiveType =  schema.ref("__Directive")
  val __TypeKindType =  schema.ref("__TypeKind")
  val __DirectiveLocationType =  schema.ref("__DirectiveLocation")

  object TypeKind extends Enumeration {
    val SCALAR, OBJECT, INTERFACE, UNION, ENUM, INPUT_OBJECT, LIST, NON_NULL = Value
    implicit val typeKindEncoder: Encoder[Value] = Encoder[String].contramap(_.toString)
  }

  implicit val directiveLocationEncoder: Encoder[Ast.DirectiveLocation] = {
    import Ast.DirectiveLocation._

    Encoder[String].contramap {
      case QUERY                  => "QUERY"
      case MUTATION               => "MUTATION"
      case SUBSCRIPTION           => "SUBSCRIPTION"
      case FIELD                  => "FIELD"
      case FRAGMENT_DEFINITION    => "FRAGMENT_DEFINITION"
      case FRAGMENT_SPREAD        => "FRAGMENT_SPREAD"
      case INLINE_FRAGMENT        => "INLINE_FRAGMENT"
      case VARIABLE_DEFINITION    => "VARIABLE_DEFINITION"

      case SCHEMA                 => "SCHEMA"
      case SCALAR                 => "SCALAR"
      case OBJECT                 => "OBJECT"
      case FIELD_DEFINITION       => "FIELD_DEFINITION"
      case ARGUMENT_DEFINITION    => "ARGUMENT_DEFINITION"
      case INTERFACE              => "INTERFACE"
      case UNION                  => "UNION"
      case ENUM                   => "ENUM"
      case ENUM_VALUE             => "ENUM_VALUE"
      case INPUT_OBJECT           => "INPUT_OBJECT"
      case INPUT_FIELD_DEFINITION => "INPUT_FIELD_DEFINITION"
    }
  }

  case class NonNullType(tpe: Type)

  val flipNullityDealias: PartialFunction[Type, Any] = {
    case NullableType(tpe) => tpe.dealias
    case tpe               => NonNullType(tpe)
  }

  val defaultTypes =
    schema.types.filterNot(_ =:= schema.queryType) ++
    List(BooleanType, IntType, FloatType, StringType, IDType)

  def interpreter(targetSchema: Schema): QueryInterpreter[Either[Throwable, *]] =
    new IntrospectionMapping(targetSchema).interpreter

  class IntrospectionMapping(targetSchema: Schema) extends ValueMapping[Either[Throwable, *]] {
    val allTypes = targetSchema.types ++ defaultTypes

    val schema = Introspection.schema

    val typeMappings =
      TypeMappings(
        ValueObjectMapping(QueryType).on[Unit](
          ValueField("__schema", _ => targetSchema),
          ValueField("__type", _ => allTypes.map(_.nullable))
        ),
        ValueObjectMapping(__SchemaType).on[Schema](
          ValueField("types", _ => allTypes.map(_.nullable)),
          ValueField("queryType", _.queryType.dealias.nullable),
          ValueField("mutationType", _.mutationType.map(_.dealias.nullable)),
          ValueField("subscriptionType", _.subscriptionType.map(_.dealias.nullable)),
          ValueField("directives", _.directives)
        ),
        ValueObjectMapping(__TypeType).on[Type](
          ValueField("kind", flipNullityDealias andThen {
            case _: ScalarType      => TypeKind.SCALAR
            case _: ObjectType      => TypeKind.OBJECT
            case _: UnionType       => TypeKind.UNION
            case _: InterfaceType   => TypeKind.INTERFACE
            case _: EnumType        => TypeKind.ENUM
            case _: InputObjectType => TypeKind.INPUT_OBJECT
            case _: ListType        => TypeKind.LIST
            case _: NonNullType     => TypeKind.NON_NULL
          }),
          ValueField("name", flipNullityDealias andThen {
            case nt: NamedType      => Some(nt.name)
            case _ => None
          }),
          ValueField("description", flipNullityDealias andThen {
            case nt: NamedType      => nt.description
            case _ => None
          }),
          ValueField("specifiedByURL", flipNullityDealias andThen {
            case s: ScalarType      => s.specifiedByURL
            case _ => None
          }),
          ValueField("fields", flipNullityDealias andThen {
            case tf: TypeWithFields => Some(tf.fields)
            case _ => None
          }),
          ValueField("interfaces", flipNullityDealias andThen {
            case tf: TypeWithFields => Some(tf.interfaces.map(_.nullable))
            case _ => None
          }),
          ValueField("possibleTypes", flipNullityDealias andThen {
            case u: UnionType       => Some(u.members.map(_.nullable))
            case i: InterfaceType   => Some(targetSchema.implementations(i).map(_.nullable))
            case _ => None
          }),
          ValueField("enumValues", flipNullityDealias andThen {
            case e: EnumType        => Some(e.enumValues)
            case _ => None
          }),
          ValueField("inputFields", flipNullityDealias andThen {
            case i: InputObjectType => Some(i.inputFields)
            case _ => None
          }),
          ValueField("ofType", flipNullityDealias andThen {
            case l: ListType        => Some(l.ofType)
            case NonNullType(t)     => Some(NullableType(t))
            case _ => None
          }),
          ValueField("isOneOf", flipNullityDealias andThen {
            case i: InputObjectType => Some(i.isOneOf)
            case _ => None
          }),
        ),
        ValueObjectMapping(__FieldType).on[Field](
          ValueField("name", _.name),
          ValueField("description", _.description),
          ValueField("args", _.args),
          ValueField("type", _.tpe.dealias),
          ValueField("isDeprecated", _.isDeprecated),
          ValueField("deprecationReason", _.deprecationReason)
        ),
        ValueObjectMapping(__InputValueType).on[InputValue](
          ValueField("name", _.name),
          ValueField("description", _.description),
          ValueField("type", _.tpe.dealias),
          ValueField("defaultValue", _.defaultValue.map(SchemaRenderer.renderValue)),
          ValueField("isDeprecated", _.isDeprecated),
          ValueField("deprecationReason", _.deprecationReason)
        ),
        ValueObjectMapping(__EnumValueType).on[EnumValueDefinition](
          ValueField("name", _.name),
          ValueField("description", _.description),
          ValueField("isDeprecated", _.isDeprecated),
          ValueField("deprecationReason", _.deprecationReason)
        ),
        ValueObjectMapping(__DirectiveType).on[DirectiveDef](
          ValueField("name", _.name),
          ValueField("description", _.description),
          ValueField("locations", _.locations),
          ValueField("args", _.args),
          ValueField("isRepeatable", _.isRepeatable)
        ),
        LeafMapping[TypeKind.Value](__TypeKindType),
        LeafMapping[Ast.DirectiveLocation](__DirectiveLocationType)
      )
  }
}
