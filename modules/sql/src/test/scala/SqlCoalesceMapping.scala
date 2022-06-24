// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import edu.gemini.grackle.syntax._

trait SqlCoalesceMapping[F[_]] extends SqlTestMapping[F] {

  object r extends TableDef("r") {
    val id = col("id", text)
  }

  object ca extends TableDef("ca") {
    val id = col("id", text)
    val rid = col("rid", text)
    val a = col("a", int4)
  }

  object cb extends TableDef("cb") {
    val id = col("id", text)
    val rid = col("rid", text)
    val b = col("b", bool)
  }

  val schema =
    schema"""
      type Query {
        r: [R!]!
      }
      type R {
        id: String!
        ca: [CA!]!
        cb: [CB!]!
      }
      type CA {
        id: String!
        a: Int!
      }
      type CB {
        id: String!
        b: Boolean!
      }
    """

  val QueryType = schema.ref("Query")
  val RType = schema.ref("R")
  val CAType = schema.ref("CA")
  val CBType = schema.ref("CB")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlRoot("r")
          )
      ),
      ObjectMapping(
        tpe = RType,
        fieldMappings =
          List(
            SqlField("id", r.id, key = true),
            SqlObject("ca", Join(r.id, ca.rid)),
            SqlObject("cb", Join(r.id, cb.rid)),
          )
      ),
      ObjectMapping(
        tpe = CAType,
        fieldMappings =
          List(
            SqlField("id", ca.id, key = true),
            SqlField("rid", ca.rid, hidden = true),
            SqlField("a", ca.a)
          )
      ),
      ObjectMapping(
        tpe = CBType,
        fieldMappings =
          List(
            SqlField("id", cb.id, key = true),
            SqlField("rid", cb.rid, hidden = true),
            SqlField("b", cb.b)
          )
      )
    )
}
