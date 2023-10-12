// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.sql.test

import grackle._
import Predicate._
import Query._
import QueryCompiler._
import Value._
import syntax._

trait SqlEmbedding2Mapping[F[_]] extends SqlTestMapping[F] {

  object ProgramTable extends TableDef("t_program") {
    val Id = col("c_program_id", varchar)
  }

  object ObservationTable extends TableDef("t_observation") {
    val Pid = col("c_program_id", varchar)
    val Id  = col("c_observation_id", varchar)
  }

  val schema = schema"""
    type Query {
      program(programId: String!): Program
    }
    type Program {
      id: String!
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
  val ProgramType                 = schema.ref("Program")
  val ObservationSelectResultType = schema.ref("ObservationSelectResult")
  val ObservationType             = schema.ref("Observation")

  val typeMappings =
    List(
      ObjectMapping(
        QueryType,
        List(
          SqlObject("program")
        )
      ),
      ObjectMapping(
        ProgramType,
        List(
          SqlField("id", ProgramTable.Id, key = true),
          SqlObject("observations")
        )
      ),
      ObjectMapping(
        ObservationSelectResultType,
        List(
          SqlField("id", ProgramTable.Id, key = true, hidden = true),
          SqlObject("matches", Join(ProgramTable.Id, ObservationTable.Pid)),
        )
      ),
      ObjectMapping(
        ObservationType,
        List(
          SqlField("id", ObservationTable.Id, key = true),
        )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "program", List(Binding("programId", StringValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(ProgramType / "id", Const(id)), child)))

    case (ObservationSelectResultType, "matches", Nil) =>
      Elab.transformChild(child => OrderBy(OrderSelections(List(OrderSelection[String](ObservationType / "id", true, true))), child))
  }
}
