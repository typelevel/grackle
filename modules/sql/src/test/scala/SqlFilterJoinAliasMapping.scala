// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.implicits._

import edu.gemini.grackle._
import syntax._
import Predicate.{Const, Eql}
import Query.{Binding, Filter, Select, Unique}
import QueryCompiler.SelectElaborator
import Value.{AbsentValue, NullValue, ObjectValue, StringValue}

trait SqlFilterJoinAliasMapping[F[_]] extends SqlTestMapping[F] {

  object episode extends TableDef("episodes3") {
    val id = col("id", varchar)
    val name = col("name", varchar)
  }

  object image extends TableDef("images3") {
    val publicUrl = col("public_url", varchar)
    val id = col("id", varchar)
    val name = col("name", varchar)
  }

  val schema =
    schema"""
      type Query {
        episode(id: String!): Episode!
      }
      type Episode {
        id: String!
        name: String!
        images(filter: Filter): [Image!]!
      }
      type Image {
        id: String!
        publicUrl: String!
        inner: Inner!
      }
      type Inner {
        name: String!
      }
      input Filter {
        name: String
      }
    """

  val QueryType = schema.ref("Query")
  val EpisodeType = schema.ref("Episode")
  val ImageType = schema.ref("Image")
  val InnerType = schema.ref("Inner")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlObject("episode")
          )
      ),
      ObjectMapping(
        tpe = EpisodeType,
        fieldMappings =
          List(
            SqlField("id", episode.id, key = true),
            SqlField("name", episode.name, key = true),
            SqlObject("images", Join(episode.id, image.id))
          )
      ),
      ObjectMapping(
        tpe = ImageType,
        fieldMappings =
          List(
            SqlField("id", image.id),
            SqlField("publicUrl", image.publicUrl, key = true),
            SqlObject("inner")
          )
      ),
      ObjectMapping(
        tpe = InnerType,
        fieldMappings =
          List(
            SqlField("name", image.name, key = true)
          )
      )
    )

  object FilterValue {
    def unapply(input: ObjectValue): Option[Predicate] = {
      input.fields match {
        case List(("name", StringValue(name))) =>
          Some(Eql(ImageType / "inner" / "name", Const(name)))
        case _ => None
      }
    }
  }

  def mkFilter(query: Query, filter: Value): Result[Query] = {
    filter match {
      case AbsentValue|NullValue => query.success
      case FilterValue(pred) => Filter(pred, query).success
      case _ => Result.failure(s"Expected filter value, found $filter")
    }
  }

  override val selectElaborator: SelectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("episode", List(Binding("id", StringValue(id))), child) =>
        Select(
          "episode",
          Nil,
          Unique(Filter(Eql(EpisodeType / "id", Const(id)), child))
        ).success
    },
    EpisodeType -> {
      case Select("images", List(Binding("filter", filter)), child) =>
        for {
          fc <- mkFilter(child, filter)
        } yield Select("images", Nil, fc)

      case other =>
        other.success
    }
  ))
}
