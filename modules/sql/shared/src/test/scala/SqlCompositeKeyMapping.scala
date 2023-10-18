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

trait SqlCompositeKeyMapping[F[_]] extends SqlTestMapping[F] {

  object compositeKeyParent extends TableDef("composite_key_parent") {
    val key1 = col("key_1", int4)
    val key2 = col("key_2", varchar)
  }

  object compositeKeyChild extends TableDef("composite_key_child") {
    val id = col("id", int4)
    val parent1 = col("parent_1", int4)
    val parent2 = col("parent_2", varchar)
  }

  val schema =
    schema"""
      type Query {
        parents: [Parent!]!
      }
      type Parent {
        key1: Int!
        key2: String!
        children: [Child!]!
      }
      type Child {
        id: Int!
        parent1: Int!
        parent2: String!
      }
    """

  val QueryType = schema.ref("Query")
  val ParentType = schema.ref("Parent")
  val ChildType = schema.ref("Child")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlObject("parents")
          )
      ),
      ObjectMapping(
        tpe = ParentType,
        fieldMappings =
          List(
            SqlField("key1", compositeKeyParent.key1, key = true),
            SqlField("key2", compositeKeyParent.key2, key = true),
            SqlObject("children", Join(List((compositeKeyParent.key1, compositeKeyChild.parent1), (compositeKeyParent.key2, compositeKeyChild.parent2))))
          )
      ),
      ObjectMapping(
        tpe = ChildType,
        fieldMappings =
          List(
            SqlField("id", compositeKeyChild.id, key = true),
            SqlField("parent1", compositeKeyChild.parent1),
            SqlField("parent2", compositeKeyChild.parent2),
          )
      )
    )
}
