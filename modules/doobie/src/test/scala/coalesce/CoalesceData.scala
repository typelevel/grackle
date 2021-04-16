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
            SqlField("id", ColumnRef("r", "id", Meta[String]), key = true),
            SqlObject("ca", Join(ColumnRef("r", "id", Meta[String]), ColumnRef("ca", "rid", Meta[String]))),
            SqlObject("cb", Join(ColumnRef("r", "id", Meta[String]), ColumnRef("cb", "rid", Meta[String]))),
          )
      ),
      ObjectMapping(
        tpe = CAType,
        fieldMappings =
          List(
            SqlField("id", ColumnRef("ca", "id", Meta[String]), key = true),
            SqlAttribute("rid", ColumnRef("ca", "rid", Meta[String])),
            SqlField("a", ColumnRef("ca", "a", Meta[Int]))
          )
      ),
      ObjectMapping(
        tpe = CBType,
        fieldMappings =
          List(
            SqlField("id", ColumnRef("cb", "id", Meta[String]), key = true),
            SqlAttribute("rid", ColumnRef("cb", "rid", Meta[String])),
            SqlField("b", ColumnRef("cb", "b", Meta[Boolean]))
          )
      )
    )
}

object CoalesceMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with CoalesceMapping[F]

}
