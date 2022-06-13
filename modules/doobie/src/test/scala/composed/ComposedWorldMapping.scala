// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.effect.Sync
import doobie.util.transactor.Transactor

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.doobie.{DoobieMappingCompanion, DoobieMonitor}

import utils.{DatabaseSuite, DoobieTestMapping}
import grackle.test.SqlComposedWorldSpec

object WorldMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping[F](transactor, monitor) with SqlWorldMapping[F]
}

object ComposedMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new SqlComposedMapping[F](WorldMapping.mkMapping(transactor, monitor), CurrencyMapping[F])
}

final class ComposedWorldSpec extends DatabaseSuite with SqlComposedWorldSpec {
  lazy val mapping = ComposedMapping.fromTransactor(xa)
}
