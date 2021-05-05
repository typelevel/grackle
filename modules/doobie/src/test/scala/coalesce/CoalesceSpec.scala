// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package coalesce

import utils.DatabaseSuite
import grackle.test.SqlCoalesceSpec
import cats.effect.IO
import edu.gemini.grackle.sql.SqlStatsMonitor
import edu.gemini.grackle.doobie.DoobieMonitor
import edu.gemini.grackle.sql.SqlMonitor
import edu.gemini.grackle.QueryExecutor
import io.circe.Json

final class CoalesceSpec extends DatabaseSuite with SqlCoalesceSpec {

  type Fragment = _root_.doobie.Fragment

  def monitor: IO[SqlStatsMonitor[IO,Fragment]] =
    DoobieMonitor.statsMonitor[IO]

  def mapping(monitor: SqlMonitor[IO,Fragment]): QueryExecutor[IO, Json] =
    CoalesceMapping.mkMapping(xa, monitor)

}
