// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package arrayjoin

import cats.effect.Sync
import doobie.Transactor
import edu.gemini.grackle.doobie._

import grackle.test.SqlArrayJoinSpec
import utils.{DatabaseSuite, DoobieTestMapping}

object ArrayJoinMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): SqlArrayJoinMapping[F] =
    new DoobieTestMapping(transactor, monitor) with SqlArrayJoinMapping[F]
}

final class ArrayJoinSpec extends DatabaseSuite with SqlArrayJoinSpec {
  lazy val mapping = ArrayJoinMapping.mkMapping(xa)
}
