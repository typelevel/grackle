// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import edu.gemini.grackle.syntax._
import org.scalatest.funsuite.AnyFunSuite
import edu.gemini.grackle.QueryExecutor
import cats.effect.IO
import io.circe.Json
import edu.gemini.grackle.sql.SqlStatsMonitor
import edu.gemini.grackle.sql.SqlMonitor
import edu.gemini.grackle.Query
import edu.gemini.grackle.Path._
import edu.gemini.grackle.Predicate._

/** Tests that confirm the compiler is writing the queries we want. */
trait SqlWorldCompilerSpec extends AnyFunSuite {

  type Fragment
  def monitor: IO[SqlStatsMonitor[IO, Fragment]]
  def mapping(monitor: SqlMonitor[IO, Fragment]): QueryExecutor[IO, Json]

  /** Expected SQL string for the simple restricted query test. */
  def simpleRestrictedQuerySql: String

  test("simple restricted query") {

    val query =
      """
        query {
          country(code: "GBR") {
            name
          }
        }
      """

    val expected =
      json"""
        {
          "data": {
            "country": {
              "name": "United Kingdom"
            }
          }
        }
      """

    val prog: IO[(Json, List[SqlStatsMonitor.SqlStats])] =
      for {
        mon <- monitor
        map  = mapping(mon)
        res <- map.compileAndRun(query)
        ss  <- mon.take
      } yield (res, ss.map(_.normalize))

    val (res, stats) = prog.unsafeRunSync()

    assert(res == expected)

    assert(
      stats == List(
        SqlStatsMonitor.SqlStats(
          Query.Unique(Eql(UniquePath(List("code")),Const("GBR")),Query.Select("name",List(),Query.Empty)),
          simpleRestrictedQuerySql,
          List("GBR"),
          1,
          2
        )
      )
    )

  }

}
