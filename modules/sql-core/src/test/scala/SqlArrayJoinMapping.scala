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

import grackle.syntax._

trait SqlArrayJoinMapping[F[_]] extends SqlTestMapping[F] {

  object root extends TableDef("array_join_root") {
    val id = col("id", varchar)
  }

  object listA extends TableDef("array_join_list_a") {
    val id = col("id", varchar)
    val rootId = col("root_id", nullable(varchar))
    val aElem = col("a_elem", nullable(list(varchar)))
  }

  object listB extends TableDef("array_join_list_b") {
    val id = col("id", varchar)
    val rootId = col("root_id", nullable(varchar))
    val bElem = col("b_elem", nullable(int4))
  }

  val schema =
    schema"""
      type Query {
        root: [Root!]!
      }
      type Root {
        id: String!
        listA: [ElemA!]!
        listB: [ElemB!]!
      }
      type ElemA {
        id: String!
        elemA: [String!]
      }
      type ElemB {
        id: String!
        elemB: Int
      }
    """

  val QueryType = schema.ref("Query")
  val RootType = schema.ref("Root")
  val ElemAType = schema.ref("ElemA")
  val ElemBType = schema.ref("ElemB")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlObject("root")
          )
      ),
      ObjectMapping(
        tpe = RootType,
        fieldMappings =
          List(
            SqlField("id", root.id, key = true),
            SqlObject("listA", Join(root.id, listA.rootId)),
            SqlObject("listB", Join(root.id, listB.rootId))
          )
      ),
      ObjectMapping(
        tpe = ElemAType,
        fieldMappings =
          List(
            SqlField("id", listA.id, key = true),
            SqlField("rootId", listA.rootId, hidden = true),
            SqlField("elemA", listA.aElem)
          )
      ),
      ObjectMapping(
        tpe = ElemBType,
        fieldMappings =
          List(
            SqlField("id", listB.id, key = true),
            SqlField("rootId", listB.rootId, hidden = true),
            SqlField("elemB", listB.bElem)
          )
      )
    )
}
