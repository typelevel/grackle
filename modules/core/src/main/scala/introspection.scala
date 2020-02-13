// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.Id

import Value._
import ScalarType._

object IntrospectionQueryInterpreter {

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
    SchemaSchema.types.filterNot(_ == SchemaSchema.queryType) ++
    List(BooleanType, IntType, FloatType, StringType, IDType)

  def apply(schema: Schema): QueryInterpreter[Id] = {
    val allTypes = schema.types ++ defaultTypes

    new DataTypeQueryInterpreter[Id](
      {
        case "__schema" =>
          (SchemaSchema.ref("__Schema"), schema)
        case "__type" =>
          (ListType(SchemaSchema.ref("__Type")), allTypes.map(_.nullable))
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

object SchemaSchema extends Schema {
  import ScalarType._

  val NameArg = InputValue("name", None, StringType, None)
  val IncludeDeprecatedArg = InputValue("includeDeprecated", None, BooleanType, Some(BooleanValue(false)))

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("__schema", None, Nil, TypeRef("__Schema"), false, None),
        Field("__type", None, List(NameArg), NullableType(TypeRef("__Type")), false, None),
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "__Schema",
      description = None,
      fields = List(
        Field("types", None, Nil, ListType(TypeRef("__Type")), false, None),
        Field("queryType", None, Nil, TypeRef("__Type"), false, None),
        Field("mutationType", None, Nil, NullableType(TypeRef("__Type")), false, None),
        Field("subscriptionType", None, Nil, NullableType(TypeRef("__Type")), false, None),
        Field("directives", None, Nil, ListType(TypeRef("__Directive")), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "__Type",
      description = None,
      fields = List(
        Field("kind", None, Nil, TypeRef("__TypeKind"), false, None),
        Field("name", None, Nil, NullableType(StringType), false, None),
        Field("description", None, Nil, NullableType(StringType), false, None),

        // OBJECT and INTERFACE only
        Field("fields", None, List(IncludeDeprecatedArg), NullableType(ListType(TypeRef("__Field"))), false, None),

        // OBJECT only
        Field("interfaces", None, Nil, NullableType(ListType(TypeRef("__Type"))), false, None),

        // INTERFACE and UNION only
        Field("possibleTypes", None, Nil, NullableType(ListType(TypeRef("__Type"))), false, None),

        // ENUM only
        Field("enumValues", None, List(IncludeDeprecatedArg), NullableType(ListType(TypeRef("__EnumValue"))), false, None),

        // INPUT_OBJECT only
        Field("inputFields", None, Nil, NullableType(ListType(TypeRef("__InputValue"))), false, None),

        // NON_NULL and LIST only
        Field("ofType", None, Nil, NullableType(TypeRef("__Type")), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "__Field",
      description = None,
      fields = List(
        Field("name", None, Nil, StringType, false, None),
        Field("description", None, Nil, NullableType(StringType), false, None),
        Field("args", None, Nil, ListType(TypeRef("__InputValue")), false, None),
        Field("type", None, Nil, TypeRef("__Type"), false, None),
        Field("isDeprecated", None, Nil, BooleanType, false, None),
        Field("deprecationReason", None, Nil, NullableType(StringType), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "__InputValue",
      description = None,
      fields = List(
        Field("name", None, Nil, StringType, false, None),
        Field("description", None, Nil, NullableType(StringType), false, None),
        Field("type", None, Nil, TypeRef("__Type"), false, None),
        Field("defaultValue", None, Nil, NullableType(StringType), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "__EnumValue",
      description = None,
      fields = List(
        Field("name", None, Nil, StringType, false, None),
        Field("description", None, Nil, NullableType(StringType), false, None),
        Field("isDeprecated", None, Nil, BooleanType, false, None),
        Field("deprecationReason", None, Nil, NullableType(StringType), false, None)
      ),
      interfaces = Nil
    ),
    EnumType(
      name = "__TypeKind",
      description = None,
      enumValues = List(
        EnumValue("SCALAR", None),
        EnumValue("OBJECT", None),
        EnumValue("INTERFACE", None),
        EnumValue("UNION", None),
        EnumValue("ENUM", None),
        EnumValue("INPUT_OBJECT", None),
        EnumValue("LIST", None),
        EnumValue("NON_NULL", None)
      )
    ),
    ObjectType(
      name = "__Directive",
      description = None,
      fields = List(
        Field("name", None, Nil, StringType, false, None),
        Field("description", None, Nil, NullableType(StringType), false, None),
        Field("locations", None, Nil, ListType(TypeRef("__DirectiveLocation")), false, None),
        Field("args", None, Nil, ListType(TypeRef("__InputValue")), false, None)
      ),
      interfaces = Nil
    ),
    EnumType(
      name = "__DirectiveLocation",
      description = None,
      enumValues = List(
        EnumValue("QUERY", None),
        EnumValue("MUTATION", None),
        EnumValue("SUBSCRIPTION", None),
        EnumValue("FIELD", None),
        EnumValue("FRAGMENT_DEFINITION", None),
        EnumValue("FRAGMENT_SPREAD", None),
        EnumValue("INLINE_FRAGMENT", None),
        EnumValue("SCHEMA", None),
        EnumValue("SCALAR", None),
        EnumValue("OBJECT", None),
        EnumValue("FIELD_DEFINITION", None),
        EnumValue("ARGUMENT_DEFINITION", None),
        EnumValue("INTERFACE", None),
        EnumValue("UNION", None),
        EnumValue("ENUM", None),
        EnumValue("ENUM_VALUE", None),
        EnumValue("INPUT_OBJECT", None),
        EnumValue("INPUT_FIELD_DEFINITION", None)
      )
    )
  )

  val directives = Nil
}
