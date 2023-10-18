// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package grackle.sql.test

import grackle.syntax._

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
