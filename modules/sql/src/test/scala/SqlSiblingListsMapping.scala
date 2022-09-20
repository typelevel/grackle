// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.implicits._
import edu.gemini.grackle.Predicate.{Const, Eql}
import edu.gemini.grackle.Query.{Binding, Filter, Select, Unique}
import edu.gemini.grackle.syntax._
import edu.gemini.grackle.QueryCompiler.SelectElaborator
import edu.gemini.grackle.Value.StringValue

trait SqlSiblingListsData[F[_]] extends SqlTestMapping[F] {

  object aTable extends TableDef("seq_scan_a") {
    val id = col("id", varchar)
  }
  object bTable extends TableDef("seq_scan_b") {
    val id = col("id", varchar)
    val aId = col("a_id", varchar)
  }
  object cTable extends TableDef("seq_scan_c") {
    val id = col("id", varchar)
    val bId = col("b_id", varchar)
    val nameC = col("name_c", varchar)
  }
  object dTable extends TableDef("seq_scan_d") {
    val id = col("id", varchar)
    val bId = col("b_id", varchar)
    val nameD = col("name_d", varchar)
  }

  val schema =
    schema"""
      type Query {
        a(id: String!): A
      }
      type A {
        id: String!
        bs: [B!]!
      }
      type B {
        id: String!
        cs: [C!]!
        ds: [D!]!
      }
      type C {
        id: String!
        nameC: String!
      }
      type D {
        id: String!
        nameD: String!
      }
    """

  val QueryType = schema.ref("Query")
  val AType = schema.ref("A")
  val BType = schema.ref("B")
  val CType = schema.ref("C")
  val DType = schema.ref("D")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlRoot("a")
          )
      ),
      ObjectMapping(
        tpe = AType,
        fieldMappings =
          List(
            SqlField("id", aTable.id, key = true, hidden = true),
            SqlObject("bs", Join(aTable.id, bTable.aId))
          )
      ),
      ObjectMapping(
        tpe = BType,
        fieldMappings =
          List(
            SqlField("id", bTable.id, key = true, hidden = true),
            SqlObject("cs", Join(bTable.id, cTable.bId)),
            SqlObject("ds", Join(bTable.id, dTable.bId))
          )
      ),
      ObjectMapping(
        tpe = CType,
        fieldMappings =
          List(
            SqlField("id", cTable.id, key = true, hidden = true),
            SqlField("nameC", cTable.nameC)
          )
      ),
      ObjectMapping(
        tpe = DType,
        fieldMappings =
          List(
            SqlField("id", dTable.id, key = true, hidden = true),
            SqlField("nameD", dTable.nameD)
          )
      )
    )

  override val selectElaborator: SelectElaborator = new SelectElaborator(
    Map(
      QueryType -> {
        case Select("a", List(Binding("id", StringValue(id))), child) =>
          Select("a", Nil, Unique(Filter(Eql(AType / "id", Const(id)), child))).rightIor
        case other => other.rightIor
      }
    )
  )

}
