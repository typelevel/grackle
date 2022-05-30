// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.{IO, Sync}
import io.circe.Json
import doobie.util.transactor.Transactor

import edu.gemini.grackle._
import sql.{SqlMonitor, SqlStatsMonitor}
import doobie.{DoobieMappingCompanion, DoobieMonitor}
import utils.{DatabaseSuite, DoobieTestMapping}
import grackle.test.{SqlWorldCompilerSpec, SqlWorldSpec}

object WorldMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping[F](transactor, monitor) with SqlWorldMapping[F]
}

final class WorldSpec extends DatabaseSuite with SqlWorldSpec {
  lazy val mapping = WorldMapping.fromTransactor(xa)
}

final class WorldCompilerSpec extends DatabaseSuite with SqlWorldCompilerSpec {

  type Fragment = _root_.doobie.Fragment

  def monitor: IO[SqlStatsMonitor[IO,Fragment]] =
    DoobieMonitor.statsMonitor[IO]

  def mapping(monitor: SqlMonitor[IO,Fragment]): QueryExecutor[IO,Json] =
    WorldMapping.mkMapping(xa, monitor)

  def simpleRestrictedQuerySql: String =
    "SELECT country.code , country.name FROM country WHERE (country.code = ?)"

  def simpleFilteredQuerySql: String =
    "SELECT city.id , city.name FROM city WHERE (city.name ILIKE ?)"
}
