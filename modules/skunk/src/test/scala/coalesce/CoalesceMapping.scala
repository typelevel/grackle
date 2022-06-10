// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package coalesce

import cats.effect.{IO, Resource, Sync}
import skunk.{AppliedFragment, Session}
import io.circe.Json

import edu.gemini.grackle.QueryExecutor
import edu.gemini.grackle.skunk.{SkunkMappingCompanion, SkunkMonitor}
import edu.gemini.grackle.sql.{SqlMonitor, SqlStatsMonitor}

import grackle.test.SqlCoalesceSpec
import utils.{DatabaseSuite, SkunkTestMapping}

object CoalesceMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): SqlCoalesceMapping[F] =
    new SkunkTestMapping[F](pool, monitor) with SqlCoalesceMapping[F]
}

final class CoalesceSpec extends DatabaseSuite with SqlCoalesceSpec {
  type Fragment = AppliedFragment

  def monitor: IO[SqlStatsMonitor[IO,Fragment]] =
    SkunkMonitor.statsMonitor[IO]

  def mapping(monitor: SqlMonitor[IO,Fragment]): QueryExecutor[IO,Json] =
    CoalesceMapping.mkMapping(pool, monitor)
}
