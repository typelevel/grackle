// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package graph

import cats.effect.Sync
import cats.implicits._
import doobie.util.meta.Meta
import doobie.util.transactor.Transactor

import edu.gemini.grackle._, doobie._
import edu.gemini.grackle.syntax._
import Query._, Path._, Predicate._, Value._
import QueryCompiler._

trait GraphMapping[F[_]] extends DoobieMapping[F] {

  object graphNode extends TableDef("graph_node") {
    val id = col("id", Meta[Int])
  }

  object graphEdge extends TableDef("graph_edge") {
    val id = col("id", Meta[Int])
    val a = col("a", Meta[Int], nullable = true)
    val b = col("b", Meta[Int], nullable = true)
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
            SqlRoot("node")
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

  override val selectElaborator: SelectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("node", List(Binding("id", IntValue(id))), child) =>
        Select("node", Nil, Unique(Filter(Eql(UniquePath(List("id")), Const(id)), child))).rightIor

      case other => other.rightIor
    }
  ))
}

object GraphMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with GraphMapping[F]
}
