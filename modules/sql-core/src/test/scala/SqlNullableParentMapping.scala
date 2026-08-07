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

import grackle.syntax._

trait SqlNullableParentMapping[F[_]] extends SqlTestMapping[F] {

  object aTable extends TableDef("nullable_parent_a") {
    val id = col("id", int4)
    val bId = col("b_id", nullable(int4))
    val name = col("name", text)
  }

  object bTable extends TableDef("nullable_parent_b") {
    val id = col("id", int4)
    val cId = col("c_id", int4)
    val name = col("name", text)
  }

  object cTable extends TableDef("nullable_parent_c") {
    val id = col("id", int4)
    val name = col("name", text)
  }

  val schema =
    schema"""
      type Query {
        as: [A!]!
      }
      type A {
        name: String!
        b: B
      }
      type B {
        name: String!
        c: C!
      }
      type C {
        name: String!
      }
    """

  val QueryType = schema.ref("Query")
  val AType = schema.ref("A")
  val BType = schema.ref("B")
  val CType = schema.ref("C")

  val typeMappings =
    TypeMappings(
      ObjectMapping(QueryType)(
        SqlObject("as")
      ),
      ObjectMapping(AType)(
        SqlField("id", aTable.id, key = true, hidden = true),
        SqlField("bId", aTable.bId, hidden = true),
        SqlField("name", aTable.name),
        SqlObject("b", Join(aTable.bId, bTable.id))
      ),
      ObjectMapping(BType)(
        SqlField("id", bTable.id, key = true, hidden = true),
        SqlField("cId", bTable.cId, hidden = true),
        SqlField("name", bTable.name),
        SqlObject("c", Join(bTable.cId, cTable.id))
      ),
      ObjectMapping(CType)(
        SqlField("id", cTable.id, key = true, hidden = true),
        SqlField("name", cTable.name)
      )
    )
}
