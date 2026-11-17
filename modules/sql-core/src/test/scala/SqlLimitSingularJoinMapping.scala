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

import grackle._
import grackle.Query.{Binding, Limit, Unique}
import grackle.QueryCompiler.{Elab, SelectElaborator}
import grackle.Value.IntValue
import grackle.syntax._

trait SqlLimitSingularJoinMapping[F[_]] extends SqlTestMapping[F] {

  object root extends TableDef("limit_singular_join_root") {
    val id = col("id", varchar)
    val name = col("name", nullable(varchar))
    val code = col("code", varchar)
  }

  object listA extends TableDef("limit_singular_join_list_a") {
    val id = col("id", varchar)
    val rootId = col("root_id", nullable(varchar))
  }

  object listB extends TableDef("limit_singular_join_list_b") {
    val id = col("id", varchar)
    val rootId = col("root_id", nullable(varchar))
  }

  object target extends TableDef("limit_singular_join_target") {
    val id = col("id", varchar)
    val name = col("name", nullable(varchar))
  }

  object assoc extends TableDef("limit_singular_join_assoc") {
    val rootId = col("root_id", varchar)
    val targetId = col("target_id", varchar)
  }

  val schema =
    schema"""
      type Query {
        roots(limit: Int!): [Root!]!
      }
      type Root {
        id: String!
        name: String
        code: String!
        lists: Lists!
        firstTarget: Target
      }
      type Lists {
        listA: [A!]!
        listB: [B!]!
      }
      type A {
        id: String!
      }
      type B {
        id: String!
      }
      type Target {
        id: String!
        name: String
      }
    """

  val QueryType = schema.ref("Query")
  val RootType = schema.ref("Root")
  val ListsType = schema.ref("Lists")
  val AType = schema.ref("A")
  val BType = schema.ref("B")
  val TargetType = schema.ref("Target")

  val typeMappings =
    TypeMappings(
      ObjectMapping(QueryType)(
        SqlObject("roots")
      ),
      ObjectMapping(RootType)(
        SqlField("id", root.id, key = true),
        SqlField("name", root.name),
        SqlField("code", root.code),
        SqlObject("lists"),
        SqlObject("firstTarget", Join(root.id, assoc.rootId), Join(assoc.targetId, target.id))
      ),
      ObjectMapping(ListsType)(
        SqlField("id", root.id, key = true, hidden = true),
        SqlObject("listA", Join(root.id, listA.rootId)),
        SqlObject("listB", Join(root.id, listB.rootId))
      ),
      ObjectMapping(AType)(
        SqlField("id", listA.id, key = true),
        SqlField("rootId", listA.rootId, hidden = true)
      ),
      ObjectMapping(BType)(
        SqlField("id", listB.id, key = true),
        SqlField("rootId", listB.rootId, hidden = true)
      ),
      ObjectMapping(TargetType)(
        SqlField("id", target.id, key = true),
        SqlField("name", target.name)
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "roots", List(Binding("limit", IntValue(n)))) =>
      Elab.transformChild(child => Limit(n, child))
    case (RootType, "firstTarget", Nil) =>
      Elab.transformChild(child => Unique(Limit(1, child)))
  }
}
