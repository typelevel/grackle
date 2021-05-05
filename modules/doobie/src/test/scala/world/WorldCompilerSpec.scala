// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import utils.DatabaseSuite
import edu.gemini.grackle.sql.{SqlStatsMonitor}
import cats.effect.IO
import edu.gemini.grackle.QueryExecutor
import edu.gemini.grackle.sql.SqlMonitor
import io.circe.Json
import grackle.test.SqlWorldCompilerSpec
import edu.gemini.grackle.doobie.DoobieMonitor

final class WorldCompilerSpec extends DatabaseSuite with SqlWorldCompilerSpec {

  type Fragment = doobie.Fragment

  def monitor: IO[SqlStatsMonitor[IO,Fragment]] =
    DoobieMonitor.statsMonitor[IO]

  def mapping(monitor: SqlMonitor[IO,Fragment]): QueryExecutor[IO,Json] =
    WorldMapping.mkMapping(xa, monitor)

  def simpleRestrictedQuerySql: String =
    "SELECT country.name, country.code FROM country WHERE (country.code = ?)"

}
