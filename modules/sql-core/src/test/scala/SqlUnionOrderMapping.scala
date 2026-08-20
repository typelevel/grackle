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
import grackle.Predicate._
import grackle.Query.{Binding, Limit, OrderBy, OrderSelection, OrderSelections}
import grackle.QueryCompiler.{Elab, SelectElaborator}
import grackle.Value.{AbsentValue, EnumValue, IntValue, NullValue}
import grackle.syntax._

// Covers a gap that predates issue #342 entirely: no fixture anywhere combines a union-typed
// field with both `order` and `limit`, which is the only condition under which
// SqlUnion.addFilterOrderByOffsetLimit pushes ordering into individual union branches
// (SqlMapping.scala: `branchOrderBy = limit.flatMap(_ => orderBy)`). On backends where
// encapsulateUnionBranch does real work (MSSQL), this is also the only path that exercises it.
// The table is schema-qualified on every backend, so union-branch encapsulation is exercised
// against a qualified name rather than only on MSSQL. The name is "union_order_entities", not
// "entities" - that name is already taken by testdata/{pg,mssql}/interfaces.sql on both backends.
trait SqlUnionOrderMapping[F[_]] extends SqlTestMapping[F] {

  object entities extends TableDef("qualified.union_order_entities") {
    val id = col("id", text)
    val entityType = col("entity_type", text)
    val name = col("name", text)
  }

  val schema =
    schema"""
      type Query {
        entities(order: Order, limit: Int): [Entity!]!
      }
      type ItemA {
        id: String!
        name: String!
      }
      type ItemB {
        id: String!
        name: String!
      }
      union Entity = ItemA | ItemB
      enum Order {
        ASC
        DESC
      }
    """

  val QueryType = schema.ref("Query")
  val ItemAType = schema.ref("ItemA")
  val ItemBType = schema.ref("ItemB")
  val EntityType = schema.ref("Entity")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlObject("entities")
        )
      ),
      SqlUnionMapping(
        tpe = EntityType,
        discriminator = entityTypeDiscriminator,
        fieldMappings = List(
          SqlField("id", entities.id, key = true, hidden = true),
          SqlField("name", entities.name, hidden = true),
          SqlField("entityType", entities.entityType, discriminator = true, hidden = true)
        )
      ),
      ObjectMapping(
        tpe = ItemAType,
        fieldMappings = List(
          SqlField("id", entities.id, key = true),
          SqlField("name", entities.name)
        )
      ),
      ObjectMapping(
        tpe = ItemBType,
        fieldMappings = List(
          SqlField("id", entities.id, key = true),
          SqlField("name", entities.name)
        )
      )
    )

  object entityTypeDiscriminator extends SqlDiscriminator {
    def discriminate(c: Cursor): Result[Type] =
      for {
        et <- c.fieldAs[String]("entityType")
      } yield et match {
        case "ItemA" => ItemAType
        case "ItemB" => ItemBType
      }

    def narrowPredicate(subtpe: Type): Result[Predicate] = {
      def mkPredicate(tpe: String): Result[Predicate] =
        Eql(EntityType / "entityType", Const(tpe)).success

      subtpe match {
        case ItemAType => mkPredicate("ItemA")
        case ItemBType => mkPredicate("ItemB")
        case _ => Result.internalError(s"Invalid discriminator: $subtpe")
      }
    }
  }

  sealed trait ListOrder {
    def ascending: Boolean
  }
  object ListOrder {
    case object Ascending extends ListOrder { def ascending = true }
    case object Descending extends ListOrder { def ascending = false }

    def fromGraphQLString(s: String): Option[ListOrder] =
      s.trim.toUpperCase match {
        case "ASC" => Some(Ascending)
        case "DESC" => Some(Descending)
        case _ => None
      }
  }

  object OrderValue {
    def unapply(ev: EnumValue): Option[ListOrder] =
      ListOrder.fromGraphQLString(ev.name)
  }

  def mkLimit(query: Query, limit: Value): Result[Query] =
    limit match {
      case AbsentValue | NullValue => query.success
      case IntValue(num) if num > 0 => Limit(num, query).success
      case IntValue(num) => Result.failure(s"Expected limit > 0, found $num")
      case other => Result.failure(s"Expected limit > 0, found $other")
    }

  def mkOrderBy(query: Query, order: Value): Result[Query] =
    order match {
      case AbsentValue | NullValue => query.success
      case OrderValue(o) =>
        OrderBy(
          OrderSelections(
            List(OrderSelection[String](EntityType / "name", ascending = o.ascending))),
          query
        ).success
      case _ => Result.failure(s"Expected order value, found $order")
    }

  override val selectElaborator = SelectElaborator {
    case (QueryType, "entities", List(Binding("order", order), Binding("limit", limit))) =>
      Elab.transformChild(child =>
        for {
          oc <- mkOrderBy(child, order)
          lc <- mkLimit(oc, limit)
        } yield lc)
  }
}
