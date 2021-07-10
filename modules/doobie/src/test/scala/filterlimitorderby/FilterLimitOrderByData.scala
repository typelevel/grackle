// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package filterlimitorderby

import cats.effect.Sync
import cats.implicits._
import doobie.Transactor
import doobie.util.meta.Meta

import edu.gemini.grackle._, doobie._, syntax._
import Path._
import Predicate.{Const, Eql}
import Query.{Binding, Filter, Limit, OrderBy, OrderSelection, OrderSelections, Select}
import QueryCompiler.SelectElaborator
import QueryInterpreter.mkErrorResult
import Value.{AbsentValue, IntValue, NullValue, ObjectValue, StringValue, TypedEnumValue}

trait FilterLimitOrderByMapping[F[_]] extends DoobieMapping[F] {

  object root extends TableDef("root") {
    val id = col("id", Meta[String])
  }

  object listA extends TableDef("lista") {
    val id = col("id", Meta[String])
    val rootId = col("root_id", Meta[String], true)
    val aElem = col("a_elem", Meta[String], true)
  }

  object listB extends TableDef("listb") {
    val id = col("id", Meta[String])
    val rootId = col("root_id", Meta[String], true)
    val bElem = col("b_elem", Meta[Int], true)
  }

  val schema =
    schema"""
      type Query {
        root(filter: Filter, limit: Int): [Root!]!
      }
      type Root {
        id: String!
        listA(filter: Filter, order: Order, limit: Int): [ElemA!]!
        listB(filter: Filter, order: Order, limit: Int): [ElemB!]!
      }
      type ElemA {
        id: String!
        elemA: String
      }
      type ElemB {
        id: String!
        elemB: Int
      }
      input Filter {
        id: String
      }
      enum Order {
        ASC
        DESC
      }
    """

  val QueryType = schema.ref("Query")
  val RootType = schema.ref("Root")
  val ElemAType = schema.ref("ElemA")
  val ElemBType = schema.ref("ElemB")

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
    def unapply(tev: TypedEnumValue): Option[ListOrder] =
      ListOrder.fromGraphQLString(tev.value.name)
  }

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlRoot("root")
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

  object FilterValue {
    def unapply(input: ObjectValue): Option[Predicate] = {
      input.fields match {
        case List(("id", StringValue(id))) =>
          Some(Eql(UniquePath(List("id")), Const(id)))
        case _ => None
      }
    }
  }

  def mkLimit(query: Query, limit: Value): Result[Query] =
    limit match {
      case AbsentValue|NullValue => query.rightIor
      case IntValue(num) if num > 0 => Limit(num, query).rightIor
      case IntValue(num) => mkErrorResult(s"Expected limit > 0, found $num")
      case other =>  mkErrorResult(s"Expected limit > 0, found $other")
    }

  def mkFilter(query: Query, filter: Value): Result[Query] = {
    filter match {
      case AbsentValue|NullValue => query.rightIor
      case FilterValue(pred) => Filter(pred, query).rightIor
      case _ => mkErrorResult(s"Expected filter value, found $filter")
    }
  }

  def mkOrderBy(query: Query, order: Value)(oss: ListOrder => List[OrderSelection[_]]): Result[Query] =
    order match {
      case AbsentValue|NullValue => query.rightIor
      case OrderValue(o) => oss(o) match {
        case Nil => query.rightIor
        case oss => OrderBy(OrderSelections(oss), query).rightIor
      }
      case _ => mkErrorResult(s"Expected sort value, found $order")
    }

  override val selectElaborator: SelectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("root", List(Binding("filter", filter), Binding("limit", limit)), child) =>
        for {
          fc <- mkFilter(child, filter)
          lc <- mkLimit(fc, limit)
        } yield Select("root", Nil, lc)

      case other => other.rightIor
    },
    RootType -> {
      case Select("listA", List(Binding("filter", filter), Binding("order", order), Binding("limit", limit)), child) =>
        for {
          fc <- mkFilter(child, filter)
          oc <- mkOrderBy(fc, order)(o => List(OrderSelection[Option[String]](UniquePath(List("elemA")), ascending = o.ascending)))
          lc <- mkLimit(oc, limit)
        } yield Select("listA", Nil, lc)

      case Select("listB", List(Binding("filter", filter), Binding("order", order), Binding("limit", limit)), child) =>
        for {
          fc <- mkFilter(child, filter)
          oc <- mkOrderBy(fc, order)(o => List(OrderSelection[Option[Int]](UniquePath(List("elemB")), ascending = o.ascending)))
          lc <- mkLimit(oc, limit)
        } yield Select("listB", Nil, lc)

      case other =>
        other.rightIor
    }
  ))
}

object FilterLimitOrderByMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): FilterLimitOrderByMapping[F] =
    new DoobieMapping(transactor, monitor) with FilterLimitOrderByMapping[F]
}
