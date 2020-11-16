// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package coalesce

import cats.effect.Sync

import edu.gemini.grackle._, skunk._
import _root_.skunk.codec.all._
import cats.effect.Resource
import _root_.skunk.Session

trait CoalesceMapping[F[_]] extends SkunkMapping[F] {

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

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SkunkRoot("r")
          )
      ),
      ObjectMapping(
        tpe = RType,
        fieldMappings =
          List(
            SkunkField("id", ColumnRef("r", "id", text), key = true),
            SkunkObject("ca", Join(ColumnRef("r", "id", text), ColumnRef("ca", "rid", text))),
            SkunkObject("cb", Join(ColumnRef("r", "id", text), ColumnRef("cb", "rid", text))),
          )
      ),
      ObjectMapping(
        tpe = CAType,
        fieldMappings =
          List(
            SkunkField("id", ColumnRef("ca", "id", text), key = true),
            SkunkAttribute("rid", ColumnRef("ca", "rid", text)),
            SkunkField("a", ColumnRef("ca", "a", int4))
          )
      ),
      ObjectMapping(
        tpe = CBType,
        fieldMappings =
          List(
            SkunkField("id", ColumnRef("cb", "id", text), key = true),
            SkunkAttribute("rid", ColumnRef("cb", "rid", text)),
            SkunkField("b", ColumnRef("cb", "b", bool))
          )
      )
    )
}

object CoalesceMapping extends TracedSkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with CoalesceMapping[F]

}