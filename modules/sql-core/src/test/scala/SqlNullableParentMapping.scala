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

  object dTable extends TableDef("nullable_parent_d") {
    val id = col("id", int4)
    val name = col("name", text)
  }

  object eTable extends TableDef("nullable_parent_e") {
    val id = col("id", int4)
    val dId = col("d_id", int4)
    val fId = col("f_id", int4)
    val name = col("name", text)
  }

  object fTable extends TableDef("nullable_parent_f") {
    val id = col("id", int4)
    val name = col("name", text)
  }

  val schema =
    schema"""
      type Query {
        as: [A!]!
        ds: [D!]!
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
      type D {
        name: String!
        es: [E!]!
      }
      type E {
        name: String!
        f: F!
      }
      type F {
        name: String!
      }
    """

  val QueryType = schema.ref("Query")
  val AType = schema.ref("A")
  val BType = schema.ref("B")
  val CType = schema.ref("C")
  val DType = schema.ref("D")
  val EType = schema.ref("E")
  val FType = schema.ref("F")

  val typeMappings =
    TypeMappings(
      ObjectMapping(QueryType)(
        SqlObject("as"),
        SqlObject("ds")
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
      ),
      ObjectMapping(DType)(
        SqlField("id", dTable.id, key = true, hidden = true),
        SqlField("name", dTable.name),
        SqlObject("es", Join(dTable.id, eTable.dId))
      ),
      ObjectMapping(EType)(
        SqlField("id", eTable.id, key = true, hidden = true),
        SqlField("dId", eTable.dId, hidden = true),
        SqlField("fId", eTable.fId, hidden = true),
        SqlField("name", eTable.name),
        SqlObject("f", Join(eTable.fId, fTable.id))
      ),
      ObjectMapping(FType)(
        SqlField("id", fTable.id, key = true, hidden = true),
        SqlField("name", fTable.name)
      )
    )
}
