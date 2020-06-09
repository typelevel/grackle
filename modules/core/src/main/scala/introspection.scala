// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.Id

import ScalarType._

object Introspection {
  val schema =
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

          # OBJECT and INTERFACE only
          fields(includeDeprecated: Boolean = false): [__Field!]

          # OBJECT only
          interfaces: [__Type!]

          # INTERFACE and UNION only
          possibleTypes: [__Type!]

          # ENUM only
          enumValues(includeDeprecated: Boolean = false): [__EnumValue!]

          # INPUT_OBJECT only
          inputFields: [__InputValue!]

          # NON_NULL and LIST only
          ofType: __Type
        }

        type __Field {
          name: String!
          description: String
          args: [__InputValue!]!
          type: __Type!
          isDeprecated: Boolean!
          deprecationReason: String
        }

        type __InputValue {
          name: String!
          description: String
          type: __Type!
          defaultValue: String
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
          args: [__InputValue!]!
        }

        enum __DirectiveLocation {
          QUERY
          MUTATION
          SUBSCRIPTION
          FIELD
          FRAGMENT_DEFINITION
          FRAGMENT_SPREAD
          INLINE_FRAGMENT
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
    ).right.get

  object TypeKind extends Enumeration {
    val SCALAR, OBJECT, INTERFACE, UNION, ENUM, INPUT_OBJECT, LIST, NON_NULL = Value
  }

  case class NonNullType(tpe: Type)

  val flipNullityDealias: PartialFunction[(Any, String), Any] = {
    case (NullableType(tpe), field) => (tpe.dealias, field)
    case (tpe: Type, field)         => (NonNullType(tpe), field)
    case (other, field)             => (other, field)
  }

  val defaultTypes =
    schema.types.filterNot(_ =:= schema.queryType) ++
    List(BooleanType, IntType, FloatType, StringType, IDType)

  def interpreter(schema: Schema): QueryInterpreter[Id] = {
    val allTypes = schema.types ++ defaultTypes

    new DataTypeQueryInterpreter[Id](
      {
        case "__schema" =>
          (Introspection.schema.ref("__Schema"), schema)
        case "__type" =>
          (ListType(Introspection.schema.ref("__Type")), allTypes.map(_.nullable))
      },
      flipNullityDealias andThen {
        case (_: Schema, "types")                => allTypes.map(_.nullable)
        case (_: Schema, "queryType")            => schema.queryType.dealias.nullable
        case (_: Schema, "mutationType")         => schema.mutationType.map(_.dealias.nullable)
        case (_: Schema, "subscriptionType")     => schema.mutationType.map(_.dealias.nullable)
        case (_: Schema, "directives")           => schema.directives

        case (_: ScalarType, "kind")             => TypeKind.SCALAR
        case (_: ObjectType, "kind")             => TypeKind.OBJECT
        case (_: UnionType, "kind")              => TypeKind.UNION
        case (_: InterfaceType, "kind")          => TypeKind.INTERFACE
        case (_: EnumType, "kind")               => TypeKind.ENUM
        case (_: InputObjectType, "kind")        => TypeKind.INPUT_OBJECT
        case (_: ListType, "kind")               => TypeKind.LIST
        case (_: NonNullType, "kind")            => TypeKind.NON_NULL

        case (t: NamedType, "name")              => Some(t.name)
        case (_: NonNullType, "name")            => None
        case (_: Type, "name")                   => None

        case (t: NamedType, "description")       => t.description
        case (_: NonNullType, "description")     => None
        case (_: Type, "description")            => None

        case (t: TypeWithFields, "fields")       => Some(t.fields)
        case (_: NonNullType, "fields")          => None
        case (_: Type, "fields")                 => None

        case (o: ObjectType, "interfaces")       => Some(o.interfaces.map(_.nullable))
        case (_: NonNullType, "interfaces")      => None
        case (_: Type, "interfaces")             => None

        case (u: UnionType, "possibleTypes")     => Some(u.members.map(_.nullable))
        case (u: InterfaceType, "possibleTypes") =>
          Some(allTypes.collect {
            case o: ObjectType if o.interfaces.exists(_ =:= u) => NullableType(o)
          })
        case (_: NonNullType, "possibleTypes")   => None
        case (_: Type, "possibleTypes")          => None

        case (e: EnumType, "enumValues")         => Some(e.enumValues)
        case (_: NonNullType, "enumValues")      => None
        case (_: Type, "enumValues")             => None

        case (i: InputObjectType, "inputFields") => Some(i.inputFields)
        case (_: Type, "inputFields")            => None

        case (l: ListType, "ofType")             => Some(l.ofType)
        case (NonNullType(t: Type), "ofType")    => Some(NullableType(t))
        case (_: Type, "ofType")                 => None

        case (f: Field, "name")                  => f.name
        case (f: Field, "description")           => f.description
        case (f: Field, "args")                  => f.args
        case (f: Field, "type")                  => f.tpe.dealias
        case (f: Field, "isDeprecated")          => f.isDeprecated
        case (f: Field, "deprecationReason")     => f.deprecationReason

        case (i: InputValue, "name")             => i.name
        case (i: InputValue, "description")      => i.description
        case (i: InputValue, "type")             => i.tpe.dealias
        case (i: InputValue, "defaultValue")     => i.defaultValue.map(SchemaRenderer.renderValue)

        case (e: EnumValue, "name")              => e.name
        case (e: EnumValue, "description")       => e.description
        case (e: EnumValue, "isDeprecated")      => e.isDeprecated
        case (e: EnumValue, "deprecationReason") => e.deprecationReason

        case (d: Directive, "name")              => d.name
        case (d: Directive, "description")       => d.description
        case (d: Directive, "locations")         => d.locations
        case (d: Directive, "args")              => d.args
      }
    )
  }
}
