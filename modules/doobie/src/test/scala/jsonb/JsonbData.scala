// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package jsonb

import doobie.{Get, Meta, Put}
import doobie.postgres.circe.jsonb.implicits._
import doobie.util.transactor.Transactor
import cats.effect.Sync
import cats.implicits._
import io.circe.Json

import edu.gemini.grackle._
import doobie._
import syntax._

import Query._
import Path._
import Predicate._
import Value._
import QueryCompiler._

trait JsonbMapping[F[_]] extends DoobieMapping[F] {

  object records extends TableDef("records") {
    val id = col("id", Meta[Int])
    val record = col("record", new Meta(Get[Json], Put[Json]), true)
  }

  val schema =
    schema"""
      type Query {
        record(id: Int!): Row
        records: [Row!]!
      }
      type Row {
        id: Int!
        record: Record
        nonNullRecord: Record!
      }
      type Record {
        bool: Boolean
        int: Int
        float: Float
        string: String
        id: ID
        choice: Choice
        arrary: [Int!]
        object: A
        children: [Child!]!
      }
      enum Choice {
        ONE
        TWO
        THREE
      }
      interface Child {
        id: ID
      }
      type A implements Child {
        id: ID
        aField: Int
      }
      type B implements Child {
        id: ID
        bField: String
      }
    """

  val QueryType = schema.ref("Query")
  val RowType = schema.ref("Row")
  val RecordType = schema.ref("Record")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlRoot("record"),
            SqlRoot("records")
          )
      ),
      ObjectMapping(
        tpe = RowType,
        fieldMappings =
          List(
            SqlField("id", records.id, key = true),
            SqlJson("record", records.record)
          )
      ),
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("record", List(Binding("id", IntValue(id))), child) =>
        Select("record", Nil, Unique(Filter(Eql(UniquePath(List("id")), Const(id)), child))).rightIor
    }
  ))
}

object JsonbMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with JsonbMapping[F]

}
