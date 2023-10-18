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

import grackle._
import grackle.syntax._
import Query._, Predicate._, Value._
import QueryCompiler._

trait SqlGraphMapping[F[_]] extends SqlTestMapping[F] {

  object graphNode extends TableDef("graph_node") {
    val id = col("id", int4)
  }

  object graphEdge extends TableDef("graph_edge") {
    val id = col("id", int4)
    val a = col("a", nullable(int4))
    val b = col("b", nullable(int4))
  }

  val schema =
    schema"""
      type Query {
        node(id: Int!): Node
      }
      type Node {
        id: Int!
        neighbours: [Node!]!
      }
      type Edge {
        id: Int!
        a: Node
        b: Node
      }
    """

  val QueryType = schema.ref("Query")
  val NodeType = schema.ref("Node")
  val EdgeType = schema.ref("Edge")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlObject("node")
          )
      ),
      ObjectMapping(
        tpe = NodeType,
        fieldMappings =
          List(
            SqlField("id", graphNode.id, key = true),
            SqlObject("neighbours", Join(graphNode.id, graphEdge.a), Join(graphEdge.b, graphNode.id))
          )
      ),
      ObjectMapping(
        tpe = EdgeType,
        fieldMappings =
          List(
            SqlField("id", graphEdge.id, key = true),
            SqlObject("a", Join(graphEdge.a, graphNode.id)),
            SqlObject("b", Join(graphEdge.b, graphNode.id))
          )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "node", List(Binding("id", IntValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(NodeType / "id", Const(id)), child)))
  }
}
