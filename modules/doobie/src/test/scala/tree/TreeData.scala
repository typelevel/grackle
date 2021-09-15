// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package tree

import cats.effect.Sync
import cats.implicits._
import doobie.util.meta.Meta
import doobie.util.transactor.Transactor

import edu.gemini.grackle._, doobie._
import edu.gemini.grackle.syntax._
import Query._, Path._, Predicate._, Value._
import QueryCompiler._

trait TreeMapping[F[_]] extends DoobieMapping[F] {

  object bintree extends TableDef("bintree") {
    val id = col("id", Meta[Int])
    val leftChild = col("left_child", Meta[Int], nullable = true)
    val rightChild = col("right_child", Meta[Int], nullable = true)
  }

  val schema =
    schema"""
      type Query {
        bintree(id: Int!): BinTree
      }
      type BinTree {
        id: Int!
        left: BinTree
        right: BinTree
      }
    """

  val QueryType = schema.ref("Query")
  val BinTreeType = schema.ref("BinTree")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlRoot("bintree")
          )
      ),
      ObjectMapping(
        tpe = BinTreeType,
        fieldMappings =
          List(
            SqlField("id", bintree.id, key = true),
            SqlObject("left", Join(bintree.leftChild, bintree.id)),
            SqlObject("right", Join(bintree.rightChild, bintree.id)),
          )
      )
    )

  override val selectElaborator: SelectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("bintree", List(Binding("id", IntValue(id))), child) =>
        Select("bintree", Nil, Unique(Filter(Eql(UniquePath(List("id")), Const(id)), child))).rightIor

      case other => other.rightIor
    }
  ))
}

object TreeMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with TreeMapping[F]
}
