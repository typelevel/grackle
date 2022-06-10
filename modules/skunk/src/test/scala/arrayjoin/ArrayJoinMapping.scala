// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package arrayjoin

import cats.effect.{Resource, Sync}
import skunk.Session
import edu.gemini.grackle.skunk._

import grackle.test.SqlArrayJoinSpec
import utils.{DatabaseSuite, SkunkTestMapping}

object ArrayJoinMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): SqlArrayJoinMapping[F] =
    new SkunkTestMapping(pool, monitor) with SqlArrayJoinMapping[F]
}

final class ArrayJoinSpec extends DatabaseSuite with SqlArrayJoinSpec {
  lazy val mapping = ArrayJoinMapping.mkMapping(pool)
}
