// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package jsonb

import doobie.util.transactor.Transactor
import cats.effect.Sync

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.doobie.{DoobieMappingCompanion, DoobieMonitor}

import utils.{DatabaseSuite, DoobieTestMapping}
import grackle.test.{SqlCursorJsonSpec, SqlJsonbSpec}

object JsonbMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping[F](transactor, monitor) with SqlJsonbMapping[F]
}

object CursorJsonMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping[F](transactor, monitor) with SqlCursorJsonMapping[F]
}

final class JsonbSpec extends DatabaseSuite with SqlJsonbSpec {
  lazy val mapping = JsonbMapping.mkMapping(xa)
}

final class CursorJsonSpec extends DatabaseSuite with SqlCursorJsonSpec {
  lazy val mapping = CursorJsonMapping.mkMapping(xa)
}
