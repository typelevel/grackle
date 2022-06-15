// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package siblinglists

import cats.effect.{Resource, Sync}
import skunk.Session

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.skunk.{SkunkMappingCompanion, SkunkMonitor}

import grackle.test.SqlSiblingListsSpec
import utils.{DatabaseSuite, SkunkTestMapping}

object SiblingListsData extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkTestMapping[F](pool, monitor) with SqlSiblingListsData[F]
}

final class SiblingListsSpec extends DatabaseSuite with SqlSiblingListsSpec {
  lazy val mapping = SiblingListsData.mkMapping(pool)
}
