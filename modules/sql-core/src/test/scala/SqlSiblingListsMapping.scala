// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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
import grackle._
import Predicate.{Const, Eql}
import Query.{Binding, Filter, Unique}
import QueryCompiler._
import Value.StringValue
import syntax._

trait SqlSiblingListsData[F[_]] extends SqlTestMapping[F] {

  object aTable extends TableDef("seq_scan_a") {
    val id = col("id", varchar)
  }
  object bTable extends TableDef("seq_scan_b") {
    val id = col("id", varchar)
    val aId = col("a_id", varchar)
  }
  object cTable extends TableDef("seq_scan_c") {
    val id = col("id", varchar)
    val bId = col("b_id", varchar)
    val nameC = col("name_c", varchar)
  }
  object dTable extends TableDef("seq_scan_d") {
    val id = col("id", varchar)
    val bId = col("b_id", varchar)
    val nameD = col("name_d", varchar)
  }

  val schema =
    schema"""
      type Query {
        a(id: String!): A
      }
      type A {
        id: String!
        bs: [B!]!
      }
      type B {
        id: String!
        cs: [C!]!
        ds: [D!]!
      }
      type C {
        id: String!
        nameC: String!
      }
      type D {
        id: String!
        nameD: String!
      }
    """

  val QueryType = schema.ref("Query")
  val AType = schema.ref("A")
  val BType = schema.ref("B")
  val CType = schema.ref("C")
  val DType = schema.ref("D")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlObject("a")
          )
      ),
      ObjectMapping(
        tpe = AType,
        fieldMappings =
          List(
            SqlField("id", aTable.id, key = true),
            SqlObject("bs", Join(aTable.id, bTable.aId))
          )
      ),
      ObjectMapping(
        tpe = BType,
        fieldMappings =
          List(
            SqlField("id", bTable.id, key = true),
            SqlObject("cs", Join(bTable.id, cTable.bId)),
            SqlObject("ds", Join(bTable.id, dTable.bId))
          )
      ),
      ObjectMapping(
        tpe = CType,
        fieldMappings =
          List(
            SqlField("id", cTable.id, key = true),
            SqlField("nameC", cTable.nameC)
          )
      ),
      ObjectMapping(
        tpe = DType,
        fieldMappings =
          List(
            SqlField("id", dTable.id, key = true),
            SqlField("nameD", dTable.nameD)
          )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "a", List(Binding("id", StringValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(AType / "id", Const(id)), child)))
  }
}
