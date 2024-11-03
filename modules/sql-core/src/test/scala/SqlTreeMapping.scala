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

trait SqlTreeMapping[F[_]] extends SqlTestMapping[F] {

  object bintree extends TableDef("bintree") {
    val id = col("id", int4)
    val leftChild = col("left_child", nullable(int4))
    val rightChild = col("right_child", nullable(int4))
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
            SqlObject("bintree")
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

  override val selectElaborator = SelectElaborator {
    case (QueryType, "bintree", List(Binding("id", IntValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(BinTreeType / "id", Const(id)), child)))
  }
}
