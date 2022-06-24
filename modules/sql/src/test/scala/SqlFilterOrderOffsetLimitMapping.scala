// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import cats.implicits._

import edu.gemini.grackle._, syntax._
import Path._
import Predicate.{Const, Eql}
import Query.{Binding, Filter, Limit, Offset, OrderBy, OrderSelection, OrderSelections, Select}
import QueryCompiler.SelectElaborator
import QueryInterpreter.mkErrorResult
import Value.{AbsentValue, IntValue, NullValue, ObjectValue, StringValue, TypedEnumValue}

trait SqlFilterOrderOffsetLimitMapping[F[_]] extends SqlTestMapping[F] {

  object root extends TableDef("root") {
    val id = col("id", varchar)
  }

  object listA extends TableDef("lista") {
    val id = col("id", varchar)
    val rootId = col("root_id", nullable(varchar))
    val aElem = col("a_elem", nullable(varchar))
  }

  object listB extends TableDef("listb") {
    val id = col("id", varchar)
    val rootId = col("root_id", nullable(varchar))
    val bElem = col("b_elem", nullable(int4))
  }

  val schema =
    schema"""
      type Query {
        root(filter: Filter, offset: Int, limit: Int): [Root!]!
      }
      type Root {
        id: String!
        listA(filter: Filter, order: Order, offset: Int, limit: Int): [ElemA!]!
        listB(filter: Filter, order: Order, offset: Int, limit: Int): [ElemB!]!
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

  def mkOffset(query: Query, limit: Value): Result[Query] =
    limit match {
      case AbsentValue|NullValue => query.rightIor
      case IntValue(num) if num == 0 => query.rightIor
      case IntValue(num) if num > 0 => Offset(num, query).rightIor
      case IntValue(num) => mkErrorResult(s"Expected offset >= 0, found $num")
      case other =>  mkErrorResult(s"Expected offset >= 0, found $other")
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
      case Select("root", List(Binding("filter", filter), Binding("offset", offset), Binding("limit", limit)), child) =>
        for {
          fc <- mkFilter(child, filter)
          oc <- mkOffset(fc, offset)
          lc <- mkLimit(oc, limit)
        } yield Select("root", Nil, lc)

      case other => other.rightIor
    },
    RootType -> {
      case Select("listA", List(Binding("filter", filter), Binding("order", order), Binding("offset", offset), Binding("limit", limit)), child) =>
        for {
          fc <- mkFilter(child, filter)
          sc <- mkOrderBy(fc, order)(o => List(OrderSelection[Option[String]](UniquePath(List("elemA")), ascending = o.ascending)))
          oc <- mkOffset(sc, offset)
          lc <- mkLimit(oc, limit)
        } yield Select("listA", Nil, lc)

      case Select("listB", List(Binding("filter", filter), Binding("order", order), Binding("offset", offset), Binding("limit", limit)), child) =>
        for {
          fc <- mkFilter(child, filter)
          sc <- mkOrderBy(fc, order)(o => List(OrderSelection[Option[Int]](UniquePath(List("elemB")), ascending = o.ascending)))
          oc <- mkOffset(sc, offset)
          lc <- mkLimit(oc, limit)
        } yield Select("listB", Nil, lc)

      case other =>
        other.rightIor
    }
  ))
}
