// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.implicits._

import edu.gemini.grackle._
import syntax._

import Query._
import Predicate._
import Value._
import QueryCompiler._

trait SqlJsonbMapping[F[_]] extends SqlTestMapping[F] {

  object records extends TableDef("records") {
    val id = col("id", int4)
    val record = col("record", nullable(jsonb))
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
            SqlObject("record"),
            SqlObject("records")
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

  override val selectElaborator = SelectElaborator {
    case (QueryType, "record", List(Binding("id", IntValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(RowType / "id", Const(id)), child)))
  }
}
