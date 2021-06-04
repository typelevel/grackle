// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import utils.DatabaseSuite
import edu.gemini.grackle.sql.{SqlStatsMonitor}
import cats.effect.IO
import edu.gemini.grackle.QueryExecutor
import edu.gemini.grackle.sql.SqlMonitor
import io.circe.Json
import skunk.AppliedFragment
import edu.gemini.grackle.skunk.SkunkMonitor
import grackle.test.SqlWorldCompilerSpec

final class WorldCompilerSpec extends DatabaseSuite with SqlWorldCompilerSpec {

  type Fragment = AppliedFragment

  def monitor: IO[SqlStatsMonitor[IO,Fragment]] =
    SkunkMonitor.statsMonitor[IO]

  def mapping(monitor: SqlMonitor[IO,Fragment]): QueryExecutor[IO,Json] =
    WorldMapping.mkMapping(pool, monitor)

  def simpleRestrictedQuerySql: String =
    "SELECT country.name, country.code FROM country WHERE (country.code = $1)"

  def simpleFilteredQuerySql: String =
    "SELECT city.name, city.id FROM city WHERE (city.name ILIKE $1)"
}
