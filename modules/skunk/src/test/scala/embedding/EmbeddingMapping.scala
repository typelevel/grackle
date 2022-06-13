// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package embedding

import cats.effect.{Resource, Sync}
import skunk.Session

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.skunk.{SkunkMappingCompanion, SkunkMonitor}

import grackle.test.SqlEmbeddingSpec
import utils.{DatabaseSuite, SkunkTestMapping}

object EmbeddingMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkTestMapping[F](pool, monitor) with SqlEmbeddingMapping[F]
}

final class EmbeddingSpec extends DatabaseSuite with SqlEmbeddingSpec {
  lazy val mapping = EmbeddingMapping.mkMapping(pool)
}
