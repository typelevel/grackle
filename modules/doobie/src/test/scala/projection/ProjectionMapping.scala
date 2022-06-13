// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package projection

import cats.effect.Sync
import doobie.Transactor

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.doobie.{DoobieMappingCompanion, DoobieMonitor}

import grackle.test.SqlProjectionSpec
import utils.{DatabaseSuite, DoobieTestMapping}

object ProjectionMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping(transactor, monitor) with SqlProjectionMapping[F]
}

final class ProjectionSpec extends DatabaseSuite with SqlProjectionSpec {
  lazy val mapping = ProjectionMapping.mkMapping(xa)
}
