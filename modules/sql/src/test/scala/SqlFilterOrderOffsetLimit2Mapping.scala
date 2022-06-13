// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package filterorderoffsetlimit

import cats.implicits._

import edu.gemini.grackle._, syntax._
import Query.{Binding, Limit, Select}
import QueryCompiler.SelectElaborator
import QueryInterpreter.mkErrorResult
import Value.{AbsentValue, IntValue, NullValue}

import utils.SqlTestMapping

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
            SqlRoot("root"),
            SqlRoot("containers")
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
      case AbsentValue|NullValue => query.rightIor
      case IntValue(num) if num > 0 => Limit(num, query).rightIor
      case IntValue(num) => mkErrorResult(s"Expected limit > 0, found $num")
      case other =>  mkErrorResult(s"Expected limit > 0, found $other")
    }

  override val selectElaborator: SelectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("root", List(Binding("limit", limit)), child) =>
        for {
          lc <- mkLimit(child, limit)
        } yield Select("root", Nil, lc)

      case Select("containers", List(Binding("limit", limit)), child) =>
        for {
          lc <- mkLimit(child, limit)
        } yield Select("containers", Nil, lc)

      case other => other.rightIor
    },
    RootType -> {
      case Select("containers", List(Binding("limit", limit)), child) =>
        for {
          lc <- mkLimit(child, limit)
        } yield Select("containers", Nil, lc)

      case Select("listA", List(Binding("limit", limit)), child) =>
        for {
          lc <- mkLimit(child, limit)
        } yield Select("listA", Nil, lc)

      case Select("listB", List(Binding("limit", limit)), child) =>
        for {
          lc <- mkLimit(child, limit)
        } yield Select("listB", Nil, lc)

      case other => other.rightIor
    },
    ContainerType -> {
      case Select("listA", List(Binding("limit", limit)), child) =>
        for {
          lc <- mkLimit(child, limit)
        } yield Select("listA", Nil, lc)

      case Select("listB", List(Binding("limit", limit)), child) =>
        for {
          lc <- mkLimit(child, limit)
        } yield Select("listB", Nil, lc)

      case other => other.rightIor
    }
  ))
}
