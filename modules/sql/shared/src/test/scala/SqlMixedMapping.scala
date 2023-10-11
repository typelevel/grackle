// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import java.util.UUID

import scala.util.Try

import cats.implicits._
import io.circe.literal._

import edu.gemini.grackle._
import syntax._
import Query._
import Predicate._
import Value._
import QueryCompiler._

// Mapping illustrating arbitrary mapping mixins with root query fields
// defined by sql, value and circe mappings.
trait SqlMixedMapping[F[_]] extends SqlTestMapping[F] with ValueMappingLike[F] { self =>
  object movies extends TableDef("movies") {
    val id = col("id", uuid)
    val title = col("title", text)
  }

  val schema =
    schema"""
        type Query {
          movie(id: UUID!): Movie
          foo: Foo!
          bar: Bar
        }
        type Movie {
          id: UUID!
          title: String!
          nested: Bar
        }
        type Foo {
          value: Int!
        }
        type Bar {
          message: String
        }
        scalar UUID
      """

  val QueryType = schema.ref("Query")
  val MovieType = schema.ref("Movie")
  val FooType = schema.ref("Foo")
  val BarType = schema.ref("Bar")
  val UUIDType = schema.ref("UUID")

  case class Foo(value: Int)

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlObject("movie"),
            ValueField[Unit]("foo", _ => Foo(23)),
            CirceField("bar", json"""{ "message": "Hello world" }""")
          )
      ),
      ObjectMapping(
        tpe = MovieType,
        fieldMappings =
          List(
            SqlField("id", movies.id, key = true),
            SqlField("title", movies.title),
            CirceField("nested", json"""{ "message": "Hello world nested" }""")
          )
      ),
      ValueObjectMapping[Foo](
        tpe = FooType,
        fieldMappings =
          List(
            ValueField("value", _.value)
          )
      ),
      LeafMapping[UUID](UUIDType)
    )

  object UUIDValue {
    def unapply(s: StringValue): Option[UUID] =
      Try(UUID.fromString(s.value)).toOption
  }

  override val selectElaborator = SelectElaborator {
    case (QueryType, "movie", List(Binding("id", UUIDValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(MovieType / "id", Const(id)), child)))
  }
}
