// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package coalesce

import cats.effect.Sync
import doobie.Transactor
import io.chrisdavenport.log4cats.Logger

import edu.gemini.grackle._, doobie._

class CoalesceMapping[F[_]: Sync](val transactor: Transactor[F], val logger: Logger[F]) extends DoobieMapping[F] {
  val schema =
    Schema(
      """
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
    ).right.get

  val QueryType = schema.ref("Query")
  val RType = schema.ref("R")
  val CAType = schema.ref("CA")
  val CBType = schema.ref("CB")

  import DoobieFieldMapping._

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            DoobieRoot("r")
          )
      ),
      ObjectMapping(
        tpe = RType,
        fieldMappings =
          List(
            DoobieField("id", ColumnRef("r", "id"), key = true),
            DoobieObject("ca", Subobject(
              List(Join(ColumnRef("r", "id"), ColumnRef("ca", "rid")))
            )),
            DoobieObject("cb", Subobject(
              List(Join(ColumnRef("r", "id"), ColumnRef("cb", "rid")))
            ))
          )
      ),
      ObjectMapping(
        tpe = CAType,
        fieldMappings =
          List(
            DoobieField("id", ColumnRef("ca", "id"), key = true),
            DoobieAttribute[String]("rid", ColumnRef("ca", "rid")),
            DoobieField("a", ColumnRef("ca", "a"))
          )
      ),
      ObjectMapping(
        tpe = CBType,
        fieldMappings =
          List(
            DoobieField("id", ColumnRef("cb", "id"), key = true),
            DoobieAttribute[String]("rid", ColumnRef("cb", "rid")),
            DoobieField("b", ColumnRef("cb", "b"))
          )
      )
    )
}

object CoalesceMapping {
  def fromTransactor[F[_] : Sync : Logger](transactor: Transactor[F]): CoalesceMapping[F] =
    new CoalesceMapping[F](transactor, Logger[F])
}
