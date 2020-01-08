// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.Id
import cats.implicits._

import Query._, Binding._, Predicate._
import QueryCompiler._

object IntrospectionQueryCompiler extends QueryCompiler(SchemaSchema) {
  val selectElaborator = new SelectElaborator(Map(
    SchemaSchema.tpe("Query").dealias -> {
      case Select("__type", List(StringBinding("name", name)), child) =>
        Select("__type", Nil, Unique(FieldEquals("name", name), child)).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

object IntrospectionQueryInterpreter {
  def apply(schema: Schema): QueryInterpreter[Id] =
    new DataTypeQueryInterpreter[Id](
      {
        case "__type" =>
          (ListType(SchemaSchema.tpe("__Type")), schema.types)
      },
      {
        case (t: NamedType, "name")        => Some(t.name)
        case (_: Type, "name")             => None
        case (t: NamedType, "description") => t.description
        case (t: TypeWithFields, "fields") => t.fields
        case (f: Field, "name")            => f.name
        case (f: Field, "description")     => f.description
        case (f: Field, "type")            => f.tpe.dealias
      }
    )
}

object SchemaSchema extends Schema {
  import ScalarType._

  val NameArg = InputValue("name", None, StringType, None)

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("__type", None, List(NameArg), TypeRef("__Type"), false, None),
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "__Type",
      description = None,
      fields = List(
        Field("name", None, Nil, NullableType(StringType), false, None),
        Field("description", None, Nil, NullableType(StringType), false, None),
        Field("fields", None, Nil, ListType(TypeRef("__Field")), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "__Field",
      description = None,
      fields = List(
        Field("name", None, Nil, StringType, false, None),
        Field("description", None, Nil, NullableType(StringType), false, None),
        Field("type", None, List(NameArg), TypeRef("__Type"), false, None)
      ),
      interfaces = Nil
    )
  )

  val directives = Nil
}
