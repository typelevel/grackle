// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package graph

import cats.effect.Sync
import doobie.util.transactor.Transactor

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.doobie.{DoobieMappingCompanion, DoobieMonitor}

import utils.{DatabaseSuite, DoobieTestMapping}
import grackle.test.SqlGraphSpec


object GraphMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping[F](transactor, monitor) with SqlGraphMapping[F]
}

final class GraphSpec extends DatabaseSuite with SqlGraphSpec {
  lazy val mapping = GraphMapping.fromTransactor(xa)
}
