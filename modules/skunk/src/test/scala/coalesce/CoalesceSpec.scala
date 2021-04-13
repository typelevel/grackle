// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package coalesce

import utils.DatabaseSuite
import grackle.test.SqlCoalesceSpec
import edu.gemini.grackle.sql.{SqlStatsMonitor}
import cats.effect.IO
import edu.gemini.grackle.QueryExecutor
import edu.gemini.grackle.sql.SqlMonitor
import io.circe.Json
import skunk.AppliedFragment
import edu.gemini.grackle.skunk.SkunkMonitor

final class CoalesceSpec extends DatabaseSuite with SqlCoalesceSpec {

  type Fragment = AppliedFragment

  def monitor: IO[SqlStatsMonitor[IO,Fragment]] =
    SkunkMonitor.statsMonitor[IO]

  def mapping(monitor: SqlMonitor[IO,Fragment]): QueryExecutor[IO,Json] =
    CoalesceMapping.mkMapping(pool, monitor)

}
