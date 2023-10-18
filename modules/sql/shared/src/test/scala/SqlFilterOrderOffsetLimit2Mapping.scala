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

import grackle._, syntax._
import Query.{Binding, Limit}
import QueryCompiler.{Elab, SelectElaborator}
import Value.{AbsentValue, IntValue, NullValue}

trait SqlFilterOrderOffsetLimit2Mapping[F[_]] extends SqlTestMapping[F] {

  object root extends TableDef("root_2") {
    val id = col("id", varchar)
  }

  object containers extends TableDef("containers_2") {
    val id = col("id", varchar)
    val rootId = col("root_id", nullable(varchar))
  }

  object listA extends TableDef("lista_2") {
    val id = col("id", varchar)
    val containerId = col("container_id", nullable(varchar))
  }

  object listB extends TableDef("listb_2") {
    val id = col("id", varchar)
    val containerId = col("container_id", nullable(varchar))
  }

  val schema =
    schema"""
      type Query {
        root(limit: Int): [Root!]!
        containers(limit: Int): [Container!]!
      }
      type Root {
        id: String!
        containers(limit: Int): [Container!]!
        listA(limit: Int): [ElemA!]!
        listB(limit: Int): [ElemB!]!
      }
      type Container {
        id: String!
        listA(limit: Int): [ElemA!]!
        listB(limit: Int): [ElemB!]!
      }
      type ElemA {
        id: String!
      }
      type ElemB {
        id: String!
      }
    """

  val QueryType = schema.ref("Query")
  val RootType = schema.ref("Root")
  val ContainerType = schema.ref("Container")
  val ElemAType = schema.ref("ElemA")
  val ElemBType = schema.ref("ElemB")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlObject("root"),
            SqlObject("containers")
          )
      ),
      ObjectMapping(
        tpe = RootType,
        fieldMappings =
          List(
            SqlField("id", root.id, key = true),
            SqlObject("containers", Join(root.id, containers.rootId)),
            SqlObject("listA", Join(root.id, containers.rootId), Join(containers.id, listA.containerId)),
            SqlObject("listB", Join(root.id, containers.rootId), Join(containers.id, listB.containerId))
          )
      ),
      ObjectMapping(
        tpe = ContainerType,
        fieldMappings =
          List(
            SqlField("id", containers.id, key = true),
            SqlObject("listA", Join(containers.id, listA.containerId)),
            SqlObject("listB", Join(containers.id, listB.containerId))
          )
      ),
      ObjectMapping(
        tpe = ElemAType,
        fieldMappings =
          List(
            SqlField("id", listA.id, key = true),
            SqlField("rootId", listA.containerId, hidden = true),
          )
      ),
      ObjectMapping(
        tpe = ElemBType,
        fieldMappings =
          List(
            SqlField("id", listB.id, key = true),
            SqlField("rootId", listB.containerId, hidden = true),
          )
      )
    )

  def mkLimit(query: Query, limit: Value): Result[Query] =
    limit match {
      case AbsentValue|NullValue => query.success
      case IntValue(num) if num > 0 => Limit(num, query).success
      case IntValue(num) => Result.failure(s"Expected limit > 0, found $num")
      case other =>  Result.failure(s"Expected limit > 0, found $other")
    }

  override val selectElaborator = SelectElaborator {
    case (QueryType, "root"|"containers", List(Binding("limit", limit))) =>
      Elab.transformChild(child => mkLimit(child, limit))

    case (RootType, "containers"|"listA"|"listB", List(Binding("limit", limit))) =>
      Elab.transformChild(child => mkLimit(child, limit))

    case (ContainerType, "listA"|"listB", List(Binding("limit", limit))) =>
      Elab.transformChild(child => mkLimit(child, limit))
  }
}
