// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import io.circe.Json
import cats.effect.IO

import edu.gemini.grackle.QueryExecutor
import edu.gemini.grackle.doobie.DoobieMonitor
import edu.gemini.grackle.sql.{SqlMonitor, SqlStatsMonitor}

import grackle.test.SqlWorldCompilerSpec
import utils.DatabaseSuite

final class WorldCompilerSpec extends DatabaseSuite with SqlWorldCompilerSpec {

  type Fragment = doobie.Fragment

  def monitor: IO[SqlStatsMonitor[IO,Fragment]] =
    DoobieMonitor.statsMonitor[IO]

  def mapping(monitor: SqlMonitor[IO,Fragment]): QueryExecutor[IO,Json] =
    WorldMapping.mkMapping(xa, monitor)

  def simpleRestrictedQuerySql: String =
    "SELECT country.code , country.name FROM country WHERE (country.code = ?)"

  def simpleFilteredQuerySql: String =
    "SELECT city.id , city.name FROM city WHERE (city.name ILIKE ?)"
}
