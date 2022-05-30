// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.{IO, Resource, Sync}
import io.circe.Json
import skunk.{AppliedFragment, Session}

import edu.gemini.grackle._
import sql.{SqlMonitor, SqlStatsMonitor}
import skunk.{SkunkMappingCompanion, SkunkMonitor}
import utils.{DatabaseSuite, SkunkTestMapping}
import grackle.test.{SqlWorldCompilerSpec, SqlWorldSpec}

object WorldMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F,Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkTestMapping[F](pool, monitor) with SqlWorldMapping[F]
}

final class WorldSpec extends DatabaseSuite with SqlWorldSpec {
  lazy val mapping = WorldMapping.mkMapping(pool)
}

final class WorldCompilerSpec extends DatabaseSuite with SqlWorldCompilerSpec {

  type Fragment = AppliedFragment

  def monitor: IO[SqlStatsMonitor[IO,Fragment]] =
    SkunkMonitor.statsMonitor[IO]

  def mapping(monitor: SqlMonitor[IO,Fragment]): QueryExecutor[IO,Json] =
    WorldMapping.mkMapping(pool, monitor)

  def simpleRestrictedQuerySql: String =
    "SELECT country.code, country.name FROM country WHERE (country.code = $1)"

  def simpleFilteredQuerySql: String =
    "SELECT city.id, city.name FROM city WHERE (city.name ILIKE $1)"
}
