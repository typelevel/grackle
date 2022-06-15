// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package jsonb

import cats.effect.{Resource, Sync}
import skunk.Session

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.skunk.{SkunkMappingCompanion, SkunkMonitor}

import utils.{DatabaseSuite, SkunkTestMapping}
import grackle.test.{SqlCursorJsonSpec, SqlJsonbSpec}

object JsonbMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkTestMapping[F](pool, monitor) with SqlJsonbMapping[F]
}

object CursorJsonMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkTestMapping[F](pool, monitor) with SqlCursorJsonMapping[F]
}

final class JsonbSpec extends DatabaseSuite with SqlJsonbSpec {
  lazy val mapping = JsonbMapping.mkMapping(pool)
}

final class CursorJsonSpec extends DatabaseSuite with SqlCursorJsonSpec {
  lazy val mapping = CursorJsonMapping.mkMapping(pool)
}
