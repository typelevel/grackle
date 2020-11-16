// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package coalesce

import cats.effect.Sync
import skunk.Transactor

import edu.gemini.grackle._, skunk._

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

  import SkunkFieldMapping._

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
            SkunkField("id", ColumnRef("r", "id"), key = true),
            SkunkObject("ca", Subobject(
              List(Join(ColumnRef("r", "id"), ColumnRef("ca", "rid")))
            )),
            SkunkObject("cb", Subobject(
              List(Join(ColumnRef("r", "id"), ColumnRef("cb", "rid")))
            ))
          )
      ),
      ObjectMapping(
        tpe = CAType,
        fieldMappings =
          List(
            SkunkField("id", ColumnRef("ca", "id"), key = true),
            SkunkAttribute[String]("rid", ColumnRef("ca", "rid")),
            SkunkField("a", ColumnRef("ca", "a"))
          )
      ),
      ObjectMapping(
        tpe = CBType,
        fieldMappings =
          List(
            SkunkField("id", ColumnRef("cb", "id"), key = true),
            SkunkAttribute[String]("rid", ColumnRef("cb", "rid")),
            SkunkField("b", ColumnRef("cb", "b"))
          )
      )
    )
}

object CoalesceMapping extends TracedSkunkMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: SkunkMonitor[F]): CoalesceMapping[F] =
    new SkunkMapping(transactor, monitor) with CoalesceMapping[F]
}
