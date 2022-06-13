// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package filterorderoffsetlimit

import cats.effect.Sync
import doobie.Transactor

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.doobie.{DoobieMappingCompanion, DoobieMonitor}

import grackle.test.{SqlFilterOrderOffsetLimitSpec, SqlFilterOrderOffsetLimit2Spec}
import utils.{DatabaseSuite, DoobieTestMapping}

object FilterOrderOffsetLimitMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping(transactor, monitor) with SqlFilterOrderOffsetLimitMapping[F]
}

object FilterOrderOffsetLimit2Mapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping(transactor, monitor) with SqlFilterOrderOffsetLimit2Mapping[F]
}

final class FilterOrderOffsetLimitSpec extends DatabaseSuite with SqlFilterOrderOffsetLimitSpec {
  lazy val mapping = FilterOrderOffsetLimitMapping.fromTransactor(xa)
}

final class FilterOrderOffsetLimit2Spec extends DatabaseSuite with SqlFilterOrderOffsetLimit2Spec {
  lazy val mapping = FilterOrderOffsetLimit2Mapping.fromTransactor(xa)
}
