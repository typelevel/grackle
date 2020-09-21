// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package jsonb

import cats.effect.Sync
import cats.implicits._
import doobie.Transactor
import io.chrisdavenport.log4cats.Logger

import edu.gemini.grackle._, doobie._
import Query._, Predicate._, Value._
import QueryCompiler._

class JsonbMapping[F[_]: Sync](val transactor: Transactor[F], val logger: Logger[F]) extends DoobieMapping[F] {
  val schema =
    Schema(
      """
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
    ).right.get

  val QueryType = schema.ref("Query")
  val RowType = schema.ref("Row")
  val RecordType = schema.ref("Record")

  import DoobieFieldMapping._

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            DoobieRoot("record"),
            DoobieRoot("records")
          )
      ),
      ObjectMapping(
        tpe = RowType,
        fieldMappings =
          List(
            DoobieField("id", ColumnRef("records", "id"), key = true),
            DoobieJson("record", ColumnRef("records", "record"))
          )
      ),
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("record", List(Binding("id", IntValue(id))), child) =>
        Select("record", Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)).rightIor
    }
  ))
}

object JsonbMapping {
  def fromTransactor[F[_] : Sync : Logger](transactor: Transactor[F]): JsonbMapping[F] =
    new JsonbMapping[F](transactor, Logger[F])
}

