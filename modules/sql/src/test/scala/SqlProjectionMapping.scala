// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import cats.implicits._

import edu.gemini.grackle._, syntax._
import Path._
import Predicate.{Const, Eql}
import Query.{Binding, Filter, Select}
import QueryCompiler.SelectElaborator
import Value.{BooleanValue, ObjectValue}

trait SqlProjectionMapping[F[_]] extends SqlTestMapping[F] {

  object level0 extends TableDef("level0") {
    val id = col("id", varchar)
  }

  object level1 extends TableDef("level1") {
    val id = col("id", varchar)
    val level0Id = col("level0_id", nullable(varchar))
  }

  object level2 extends TableDef("level2") {
    val id = col("id", varchar)
    val level1Id = col("level1_id", nullable(varchar))
    val attr = col("attr", nullable(bool))
  }

  val schema =
    schema"""
      type Query {
        level0(filter: Filter): [Level0!]!
        level1(filter: Filter): [Level1!]!
        level2(filter: Filter): [Level2!]!
      }
      type Level0 {
        id: String!
        level1(filter: Filter): [Level1!]!
      }
      type Level1 {
        id: String!
        level2(filter: Filter): [Level2!]!
      }
      type Level2 {
        id: String!
        attr: Boolean
      }
      input Filter {
        attr: Boolean
      }
    """

  val QueryType = schema.ref("Query")
  val Level0Type = schema.ref("Level0")
  val Level1Type = schema.ref("Level1")
  val Level2Type = schema.ref("Level2")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlRoot("level0"),
            SqlRoot("level1"),
            SqlRoot("level2")
          )
      ),
      ObjectMapping(
        tpe = Level0Type,
        fieldMappings =
          List(
            SqlField("id", level0.id, key = true),
            SqlObject("level1", Join(level0.id, level1.level0Id))
          )
      ),
      ObjectMapping(
        tpe = Level1Type,
        fieldMappings =
          List(
            SqlField("id", level1.id, key = true),
            SqlField("level0_id", level1.level0Id, hidden = true),
            SqlObject("level2", Join(level1.id, level2.level1Id))
          )
      ),
      ObjectMapping(
        tpe = Level2Type,
        fieldMappings =
          List(
            SqlField("id", level2.id, key = true),
            SqlField("attr", level2.attr),
            SqlField("level1_id", level2.level1Id, hidden = true)
          )
      )
    )

  object Level0FilterValue {
    def unapply(input: ObjectValue): Option[Predicate] = {
      input.fields match {
        case List(("attr", BooleanValue(attr))) =>
          Some(Eql(UniquePath(List("level1", "level2", "attr")), Const(Option(attr))))
        case _ => None
      }
    }
  }

  object Level1FilterValue {
    def unapply(input: ObjectValue): Option[Predicate] = {
      input.fields match {
        case List(("attr", BooleanValue(attr))) =>
          Some(Eql(UniquePath(List("level2", "attr")), Const(Option(attr))))
        case _ => None
      }
    }
  }

  object Level2FilterValue {
    def unapply(input: ObjectValue): Option[Predicate] = {
      input.fields match {
        case List(("attr", BooleanValue(attr))) =>
          Some(Eql(UniquePath(List("attr")), Const(Option(attr))))
        case _ => None
      }
    }
  }

  override val selectElaborator: SelectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("level0", List(Binding("filter", filter)), child) =>
        val f = filter match {
          case Level0FilterValue(f) => Filter(f, child)
          case _ => child
        }
        Select("level0", Nil, f).rightIor

      case Select("level1", List(Binding("filter", filter)), child) =>
        val f = filter match {
          case Level1FilterValue(f) => Filter(f, child)
          case _ => child
        }
        Select("level1", Nil, f).rightIor

      case Select("level2", List(Binding("filter", filter)), child) =>
        val f = filter match {
          case Level2FilterValue(f) => Filter(f, child)
          case _ => child
        }
        Select("level2", Nil, f).rightIor

      case other => other.rightIor
    },
    Level0Type -> {
      case Select("level1", List(Binding("filter", filter)), child) =>
        val f = filter match {
          case Level1FilterValue(f) => Filter(f, child)
          case _ => child
        }
        Select("level1", Nil, f).rightIor

      case other => other.rightIor
    },
    Level1Type -> {
      case Select("level2", List(Binding("filter", filter)), child) =>
        val f = filter match {
          case Level2FilterValue(f) => Filter(f, child)
          case _ => child
        }
        Select("level2", Nil, f).rightIor

      case other => other.rightIor
    }
  ))
}
