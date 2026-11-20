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

import cats.Order
import cats.implicits._

import grackle._
import grackle.Predicate.{Const, Gt}
import grackle.Query.{Binding, Filter, Limit, OrderBy, OrderSelection, OrderSelections}
import grackle.QueryCompiler.{Elab, SelectElaborator}
import grackle.Value.{AbsentValue, BooleanValue, IntValue, NullValue, StringValue}
import grackle.syntax._

// Items are keyed by a number held as text, so the database orders them as text ("10" before
// "9") unless `orderingToFragment` has it order them as the numbers they are.
trait SqlOrderingRewriteMapping[F[_]] extends SqlTestMapping[F] {

  object groups extends TableDef("numbered_groups") {
    val id = col("id", int4)
  }

  object items extends TableDef("numbered_items") {
    val code = col("code", varchar)
    val groupId = col("group_id", nullable(int4))
  }

  override def orderingToFragment(col: SqlColumn): Option[Fragment => Fragment] =
    if (col.column == "code")
      Some(ref => Fragments.const("CAST(") |+| ref |+| Fragments.const(" AS DECIMAL(10))"))
    else None

  val numeric: Order[String] = Order.by(BigInt(_))

  val schema =
    schema"""
      type Query {
        items(after: String, limit: Int, descending: Boolean): [Item!]!
        groups: [Group!]!
      }
      type Group {
        id: Int!
        items(limit: Int): [Item!]!
      }
      type Item {
        code: String!
      }
    """

  val QueryType = schema.ref("Query")
  val GroupType = schema.ref("Group")
  val ItemType = schema.ref("Item")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlObject("items"),
          SqlObject("groups")
        )
      ),
      ObjectMapping(
        tpe = GroupType,
        fieldMappings = List(
          SqlField("id", groups.id, key = true),
          SqlObject("items", Join(groups.id, items.groupId))
        )
      ),
      ObjectMapping(
        tpe = ItemType,
        fieldMappings = List(
          SqlField("code", items.code, key = true),
          SqlField("groupId", items.groupId, hidden = true)
        )
      )
    )

  def mkLimit(query: Query, limit: Value): Result[Query] =
    limit match {
      case AbsentValue | NullValue => query.success
      case IntValue(num) if num > 0 => Limit(num, query).success
      case other => Result.failure(s"Expected limit > 0, found $other")
    }

  def mkAfter(query: Query, after: Value): Result[Query] =
    after match {
      case AbsentValue | NullValue => query.success
      case StringValue(code) => Filter(Gt(ItemType / "code", Const(code))(numeric), query).success
      case other => Result.failure(s"Expected a code, found $other")
    }

  def mkAscending(descending: Value): Result[Boolean] =
    descending match {
      case AbsentValue | NullValue => true.success
      case BooleanValue(desc) => (!desc).success
      case other => Result.failure(s"Expected a boolean, found $other")
    }

  def byCode(query: Query, ascending: Boolean): Query =
    OrderBy(
      OrderSelections(
        List(OrderSelection[String](ItemType / "code", ascending = ascending)(numeric))),
      query)

  override val selectElaborator = SelectElaborator {
    case (
          QueryType,
          "items",
          List(
            Binding("after", after),
            Binding("limit", limit),
            Binding("descending", descending))) =>
      Elab.transformChild(child =>
        for {
          fc <- mkAfter(child, after)
          asc <- mkAscending(descending)
          lc <- mkLimit(byCode(fc, asc), limit)
        } yield lc)

    case (QueryType, "groups", Nil) =>
      Elab.transformChild(child =>
        OrderBy(OrderSelections(List(OrderSelection[Int](GroupType / "id"))), child))

    case (GroupType, "items", List(Binding("limit", limit))) =>
      Elab.transformChild(child => mkLimit(byCode(child, true), limit))
  }
}
