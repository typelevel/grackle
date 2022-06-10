// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package coalesce

import cats.effect.{IO, Sync}
import doobie.util.transactor.Transactor
import io.circe.Json

import edu.gemini.grackle.QueryExecutor
import edu.gemini.grackle.doobie.{DoobieMappingCompanion, DoobieMonitor}
import edu.gemini.grackle.sql.{SqlMonitor, SqlStatsMonitor}

import grackle.test.SqlCoalesceSpec
import utils.{DatabaseSuite, DoobieTestMapping}

object CoalesceMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): SqlCoalesceMapping[F] =
    new DoobieTestMapping[F](transactor, monitor) with SqlCoalesceMapping[F]
}

final class CoalesceSpec extends DatabaseSuite with SqlCoalesceSpec {
  type Fragment = _root_.doobie.Fragment

  def monitor: IO[SqlStatsMonitor[IO,Fragment]] =
    DoobieMonitor.statsMonitor[IO]

  def mapping(monitor: SqlMonitor[IO, Fragment]): QueryExecutor[IO, Json] =
    CoalesceMapping.mkMapping(xa, monitor)
}
