// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

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

  val QueryType = schema.queryType
  val __SchemaType = schema.ref("__Schema")
  val __TypeType =  schema.ref("__Type")
  val __FieldType =  schema.ref("__Field")
  val __InputValueType =  schema.ref("__InputValue")
  val __EnumValueType =  schema.ref("__EnumValue")
  val __DirectiveType =  schema.ref("__Directive")
  val __TypeKindType =  schema.ref("__TypeKind")

  object TypeKind extends Enumeration {
    val SCALAR, OBJECT, INTERFACE, UNION, ENUM, INPUT_OBJECT, LIST, NON_NULL = Value
    implicit val typeKindEncoder: Encoder[Value] = Encoder[String].contramap(_.toString)
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
      List(
        ValueObjectMapping[Unit](
          tpe = QueryType,
          fieldMappings =
            List(
              ValueField("__schema", _ => targetSchema),
              ValueField("__type", _ => allTypes.map(_.nullable))
            )
        ),
        ValueObjectMapping[Schema](
          tpe = __SchemaType,
          fieldMappings =
            List(
              ValueField("types", _ => allTypes.map(_.nullable)),
              ValueField("queryType", _.queryType.dealias.nullable),
              ValueField("mutationType", _.mutationType.map(_.dealias.nullable)),
              ValueField("subscriptionType", _.subscriptionType.map(_.dealias.nullable)),
              ValueField("directives", _.directives)
            )
        ),
        ValueObjectMapping[Type](
          tpe = __TypeType,
          fieldMappings =
            List(
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
              ValueField("fields", flipNullityDealias andThen {
                case tf: TypeWithFields => Some(tf.fields)
                case _ => None
              }),
              ValueField("interfaces", flipNullityDealias andThen {
                case ot: ObjectType     => Some(ot.interfaces.map(_.nullable))
                case _ => None
              }),
              ValueField("possibleTypes", flipNullityDealias andThen {
                case u: UnionType       => Some(u.members.map(_.nullable))
                case i: InterfaceType   =>
                  Some(allTypes.collect {
                    case o: ObjectType if o.interfaces.exists(_ =:= i) => NullableType(o)
                  })
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
              })
            )
        ),
        ValueObjectMapping[Field](
          tpe = __FieldType,
          fieldMappings =
            List(
              ValueField("name", _.name),
              ValueField("description", _.description),
              ValueField("args", _.args),
              ValueField("type", _.tpe.dealias),
              ValueField("isDeprecated", _.isDeprecated),
              ValueField("deprecationReason", _.deprecationReason)
            )
        ),
        ValueObjectMapping[InputValue](
          tpe = __InputValueType,
          fieldMappings =
            List(
              ValueField("name", _.name),
              ValueField("description", _.description),
              ValueField("type", _.tpe.dealias),
              ValueField("defaultValue", _.defaultValue.map(SchemaRenderer.renderValue))
            )
        ),
        ValueObjectMapping[EnumValue](
          tpe = __EnumValueType,
          fieldMappings =
            List(
              ValueField("name", _.name),
              ValueField("description", _.description),
              ValueField("isDeprecated", _.isDeprecated),
              ValueField("deprecationReason", _.deprecationReason)
            )
        ),
        ValueObjectMapping[Directive](
          tpe = __DirectiveType,
          fieldMappings =
            List(
              ValueField("name", _.name),
              ValueField("description", _.description),
              ValueField("locations", _.locations),
              ValueField("args", _.args),
              ValueField("isRepeatable", _.isRepeatable)
            )
        ),
        LeafMapping[TypeKind.Value](__TypeKindType)
    )
  }
}
