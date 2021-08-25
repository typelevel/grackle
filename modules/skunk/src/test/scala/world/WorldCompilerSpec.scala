// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.IO
import io.circe.Json
import skunk.AppliedFragment

import edu.gemini.grackle._
import sql.{SqlMonitor, SqlStatsMonitor}
import skunk.SkunkMonitor

import grackle.test.SqlWorldCompilerSpec
import utils.DatabaseSuite

final class WorldCompilerSpec extends DatabaseSuite with SqlWorldCompilerSpec {

  type Fragment = AppliedFragment

  def monitor: IO[SqlStatsMonitor[IO,Fragment]] =
    SkunkMonitor.statsMonitor[IO]

  def mapping(monitor: SqlMonitor[IO,Fragment]): QueryExecutor[IO,Json] =
    WorldMapping.mkMapping(pool, monitor)

  def simpleRestrictedQuerySql: String =
    "SELECT country.name, country.code FROM (SELECT country.name, country.code FROM country INNER JOIN (SELECT DISTINCT country.code FROM country WHERE (country.code IS NOT NULL) AND (country.code = $1))AS pred_0 ON (pred_0.code = country.code) )AS country"

  def simpleFilteredQuerySql: String =
    "SELECT city.name, city.id FROM (SELECT city.name, city.id FROM city INNER JOIN (SELECT DISTINCT city.id FROM city WHERE (city.id IS NOT NULL) AND (city.name ILIKE $1))AS pred_0 ON (pred_0.id = city.id) )AS city"
}
