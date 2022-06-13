// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package embedding

import cats.effect.Sync
import doobie.Transactor

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.doobie.{DoobieMappingCompanion, DoobieMonitor}

import grackle.test.SqlEmbeddingSpec
import utils.{DatabaseSuite, DoobieTestMapping}

object EmbeddingMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping[F](transactor, monitor) with SqlEmbeddingMapping[F]
}

final class EmbeddingSpec extends DatabaseSuite with SqlEmbeddingSpec {
  lazy val mapping = EmbeddingMapping.mkMapping(xa)
}
