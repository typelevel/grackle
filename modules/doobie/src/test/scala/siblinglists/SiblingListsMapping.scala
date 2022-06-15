// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package siblinglists

import cats.effect.Sync
import doobie.util.transactor.Transactor

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.doobie.{DoobieMappingCompanion, DoobieMonitor}

import grackle.test.SqlSiblingListsSpec
import utils.{DatabaseSuite, DoobieTestMapping}

object SiblingListsData extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping[F](transactor, monitor) with SqlSiblingListsData[F]
}

final class SiblingListsSpec extends DatabaseSuite with SqlSiblingListsSpec {
  lazy val mapping = SiblingListsData.mkMapping(xa)
}
