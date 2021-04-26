// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package coalesce

import _root_.doobie.util.meta.Meta
import _root_.doobie.util.transactor.Transactor
import cats.effect.Sync
import edu.gemini.grackle._
import edu.gemini.grackle.doobie._
import edu.gemini.grackle.syntax._

trait CoalesceMapping[F[_]] extends DoobieMapping[F] {

  object r extends TableDef("r") {
    val id = col("id", Meta[String])
  }

  object ca extends TableDef("ca") {
    val id = col("id", Meta[String])
    val rid = col("rid", Meta[String])
    val a = col("a", Meta[Int])
  }

  object cb extends TableDef("cb") {
    val id = col("id", Meta[String])
    val rid = col("rid", Meta[String])
    val b = col("b", Meta[Boolean])
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

object CoalesceMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with CoalesceMapping[F]

}
