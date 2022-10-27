// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import edu.gemini.grackle.syntax._

trait SqlEmbedding3Mapping[F[_]] extends SqlTestMapping[F] {

  object Bogus extends RootDef {
    val Id = col("<bogus>", varchar)
  }

  object ObservationTable extends TableDef("t_observation") {
    val Pid = col("c_program_id", varchar)
    val Id  = col("c_observation_id", varchar)
  }

  val schema = schema"""
    type Query {
      observations: ObservationSelectResult!
    }
    type ObservationSelectResult {
      matches: [Observation!]!
    }
    type Observation {
      id: String!
    }
  """

  val QueryType                   = schema.ref("Query")
  val ObservationSelectResultType = schema.ref("ObservationSelectResult")
  val ObservationType             = schema.ref("Observation")

  val typeMappings =
    List(
      ObjectMapping(
        QueryType,
        List(
          SqlObject("observations")
        )
      ),
      ObjectMapping(
        ObservationSelectResultType,
        List(
          SqlField("<key>", Bogus.Id, hidden = true),
          SqlObject("matches"),
        )
      ),
      ObjectMapping(
        ObservationType,
        List(
          SqlField("id", ObservationTable.Id, key = true),
        )
      )
    )

}
