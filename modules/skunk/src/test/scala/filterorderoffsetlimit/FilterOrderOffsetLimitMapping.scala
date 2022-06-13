// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package filterorderoffsetlimit

import cats.effect.{Resource, Sync}
import skunk.Session

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.skunk.{SkunkMappingCompanion, SkunkMonitor}

import grackle.test.{SqlFilterOrderOffsetLimitSpec, SqlFilterOrderOffsetLimit2Spec}
import utils.{DatabaseSuite, SkunkTestMapping}

object FilterOrderOffsetLimitMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkTestMapping(pool, monitor) with SqlFilterOrderOffsetLimitMapping[F]
}

object FilterOrderOffsetLimit2Mapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkTestMapping(pool, monitor) with SqlFilterOrderOffsetLimit2Mapping[F]
}

final class FilterOrderOffsetLimitSpec extends DatabaseSuite with SqlFilterOrderOffsetLimitSpec {
  lazy val mapping = FilterOrderOffsetLimitMapping.mkMapping(pool)
}

final class FilterOrderOffsetLimit2Spec extends DatabaseSuite with SqlFilterOrderOffsetLimit2Spec {
  lazy val mapping = FilterOrderOffsetLimit2Mapping.mkMapping(pool)
}
