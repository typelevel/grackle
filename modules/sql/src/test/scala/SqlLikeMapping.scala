// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.implicits._

import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import Query._, Predicate._, Value._
import QueryCompiler._
import sql.Like

trait SqlLikeMapping[F[_]] extends SqlTestMapping[F] {

  object likes extends TableDef("likes") {
    val id = col("id", int4)
    val notNullableText = col("notnullable", text)
    val nullableText = col("nullable", nullable(text))
  }

  val schema =
    schema"""
      type Query {
        likes: [Like!]!
        likeNotNullableNotNullable(pattern: String!): [Like!]!
        likeNotNullableNullable(pattern: String): [Like!]!
        likeNullableNotNullable(pattern: String!): [Like!]!
        likeNullableNullable(pattern: String): [Like!]!
      }
      type Like {
        id: Int!
        notNullable: String!
        nullable: String
      }
    """

  val QueryType = schema.ref("Query")
  val LikeType = schema.ref("Like")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlRoot("likes"),
          )
      ),
      ObjectMapping(
        tpe = LikeType,
        fieldMappings =
          List(
            SqlField("id", likes.id, key = true),
            SqlField("notNullable", likes.notNullableText),
            SqlField("nullable", likes.nullableText)
          )
      )
    )

  object NonNullablePattern {
    def unapply(value: Value): Option[String] =
      value match {
        case AbsentValue => None
        case StringValue(p) => Some(p)
        case other => sys.error(s"Expected pattern, found $other")
      }
  }

  object NullablePattern {
    def unapply(value: Value): Option[Option[String]] =
      value match {
        case AbsentValue => None
        case NullValue => Some(None)
        case StringValue(p) => Some(Some(p))
        case other => sys.error(s"Expected pattern, found $other")
      }
  }

  def mkPredicate(t: Term[Option[String]], pattern: Option[String]): Predicate =
    pattern match {
      case None => IsNull(t, true)
      case Some(p) => Like(t, p, false)
    }

  override val selectElaborator: SelectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select(f@"likeNotNullableNotNullable", List(Binding("pattern", NonNullablePattern(pattern))), child) =>
        Rename(f, Select("likes", Nil, Filter(Like(LikeType / "notNullable", pattern, false), child))).rightIor
      case Select(f@"likeNotNullableNullable", List(Binding("pattern", NullablePattern(pattern))), child) =>
        Rename(f, Select("likes", Nil, Filter(mkPredicate(LikeType / "notNullable", pattern), child))).rightIor
      case Select(f@"likeNullableNotNullable", List(Binding("pattern", NonNullablePattern(pattern))), child) =>
        Rename(f, Select("likes", Nil, Filter(Like(LikeType / "nullable", pattern, false), child))).rightIor
      case Select(f@"likeNullableNullable", List(Binding("pattern", NullablePattern(pattern))), child) =>
        Rename(f, Select("likes", Nil, Filter(mkPredicate(LikeType / "nullable", pattern), child))).rightIor

      case other => other.rightIor
    }
  ))
}
