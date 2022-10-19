// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Cursor.ListTransformCursor
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.syntax._
import edu.gemini.grackle.QueryCompiler.SelectElaborator
import edu.gemini.grackle.Value._
import edu.gemini.grackle.Predicate._

trait SqlRobMapping[F[_]] extends SqlTestMapping[F] {

  object ProgramTable extends TableDef("t_program") {
    val Id      = col("c_program_id", varchar)
  }

  object ObservationTable extends TableDef("t_observation") {
    val ProgramId = col("c_program_id", varchar)
    val Id = col("c_observation_id", varchar)
  }

  val schema =
    schema"""
      type Query {
        program(programId: String!): Program
      }
      type Program {
        id: String!
        observations(limit: Int): ObservationSelectResult!
      }
      type ObservationSelectResult {
        matches: [Observation!]!
        hasMore: Boolean!
      }
      type Observation {
        id: String!
      }
    """

  val QueryType = schema.ref("Query")
  val ProgramType = schema.ref("Program")
  val ObsevationSelectResultType = schema.ref("ObservationSelectResult")
  val ObservationType = schema.ref("Observation")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlObject("program"),
          )
      ),
      ObjectMapping(
        tpe = ProgramType,
        fieldMappings =
          List(
            SqlField("id", ProgramTable.Id, key = true),
            SqlObject("observations")
          ),
      ),
      ObjectMapping(
        tpe = ObsevationSelectResultType,
        fieldMappings =
          List(
            SqlField("<key>", ProgramTable.Id, key = true, hidden = true),
            SqlObject("matches", Join(ProgramTable.Id, ObservationTable.ProgramId)),
            CursorField("hasMore", hasMore),
          )
      ),
      ObjectMapping(
        tpe = ObservationType,
        fieldMappings =
          List(
            SqlField("id", ObservationTable.Id, key = true),
            SqlField("<program-id>", ObservationTable.ProgramId, hidden = true),
          )
      )
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("program", List(Binding("programId", StringValue(id))), child) =>
        Result(Select("program", Nil, Unique(Filter(Eql(ProgramType / "id", Const(id)), child))))
    },
    ProgramType -> {
      case Select("observations", List(Binding("limit", IntValue(lim))), child) => {
        selectResult("observations", child, lim) { q =>
          FilterOrderByOffsetLimit(
            pred   = None,
            oss    = Some(List(OrderSelection[Int](ObservationType / "id", true, true))),
            offset = None,
            limit  = Some(lim + 1),
            child  = q
          )
        }
      }
    },
  ))

  // keys for things we expect to find in the environment
  val LimitKey = "selectResultLimit"
  val AliasKey = "selectResultAlias"

  def hasMore(c: Cursor): Result[Boolean] =
    for {
      limit   <- c.envR[Int](LimitKey)
      alias   <- c.envR[Option[String]](AliasKey)
      matches <- c.field("matches", alias)
      size    <- matches.listSize
    } yield limit < 0 || size > limit

  /** A cursor transformation that takes `n` elements from a list (if it's a list). */
  object Take {
    def apply(n: Int)(c: Cursor): Result[Cursor] =
      (c.listSize, c.asList(Seq)).mapN { (size, elems) =>
        if (size <= n) c
        else ListTransformCursor(c, size - 1, elems.init)
      }
  }
 
  implicit class QueryOps(self: Query.type) {
    def mapSomeFields(query: Query)(f: PartialFunction[Query, Result[Query]]): Result[Query] = 
      self.mapFields(query)(q => f.applyOrElse(q, Result.apply[Query]))
  }

  /**
   * Transform a top-level `Select(field, ..., child)` that yields a SelectResult with a specified
   * `limit` into the proper form. The "matches" subselect's child is passed to `transform`, which
   * is how you add filter, ordering, limit (MUST BE `limit + 1`!) and so on as if it were a
   * top-level query without the SelectResult structure. See `TargetMapping` for instance, for an 
   * example. Note that this will fail if there is no "items" subquery. Supporting such queries
   * would complicate things and isn't really necessary.
   */
  def selectResult(field: String, child: Query, limit: Int)(transform: Query => Query): Result[Query] = {

    // Find the "items" node under the main query and add all our filtering
    // and whatnot down in there, wrapping with a transform that removes the last row from the
    // final results.
    def transformMatches(q: Query): Result[Query] =
      Query.mapSomeFields(q) {
        case Select("matches", Nil, child) =>
          Result(Select("matches", Nil, Query.TransformCursor(Take(limit), transform(child))))
      }

    // If we're selecting "matches" then continue by transforming the child query, otherwise
    // punt because there's really no point in doing such a selection.
    if (!Query.hasField(child, "matches")) Result.failure("Field `matches` must be selected.") // meh
    else
      transformMatches(child).map { child =>
        Select(field, Nil,
          Environment(
            Env(
              LimitKey -> limit,
              AliasKey -> Query.fieldAlias(child, "matches"),
            ),
            child
          )
        )
      }

  }

}