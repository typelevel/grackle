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

package grackle.sql.test

import cats.implicits._

import grackle._, syntax._
import Predicate.{Const, Eql}
import Query.{Binding, Filter}
import QueryCompiler._
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
            SqlObject("level0"),
            SqlObject("level1"),
            SqlObject("level2")
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
          Some(Eql(Level0Type / "level1" / "level2" / "attr", Const(Option(attr))))
        case _ => None
      }
    }
  }

  object Level1FilterValue {
    def unapply(input: ObjectValue): Option[Predicate] = {
      input.fields match {
        case List(("attr", BooleanValue(attr))) =>
          Some(Eql(Level1Type / "level2" / "attr", Const(Option(attr))))
        case _ => None
      }
    }
  }

  object Level2FilterValue {
    def unapply(input: ObjectValue): Option[Predicate] = {
      input.fields match {
        case List(("attr", BooleanValue(attr))) =>
          Some(Eql(Level2Type / "attr", Const(Option(attr))))
        case _ => None
      }
    }
  }

  override val selectElaborator = SelectElaborator {
    case (QueryType, "level0", List(Binding("filter", Level0FilterValue(f)))) =>
      Elab.transformChild(child => Filter(f, child))

    case (QueryType, "level1", List(Binding("filter", Level1FilterValue(f)))) =>
      Elab.transformChild(child => Filter(f, child))

    case (QueryType, "level2", List(Binding("filter", Level2FilterValue(f)))) =>
      Elab.transformChild(child => Filter(f, child))

    case (Level0Type, "level1", List(Binding("filter", Level1FilterValue(f)))) =>
      Elab.transformChild(child => Filter(f, child))

    case (Level1Type, "level2", List(Binding("filter", Level2FilterValue(f)))) =>
      Elab.transformChild(child => Filter(f, child))
  }
}
