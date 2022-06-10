// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package scalars

import cats.effect.{Resource, Sync}
import skunk.Session
import skunk.codec.{all => codec}

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.skunk.{SkunkMappingCompanion, SkunkMonitor}
import grackle.test.SqlMovieSpec
import utils.{DatabaseSuite, SkunkTestMapping}

object MovieMapping extends SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkTestMapping[F](pool, monitor) with SqlMovieMapping[F] {
      def genre: Codec =
        (codec.int4.imap(Genre.fromInt)(Genre.toInt), false)

      def feature: Codec =
        (codec.varchar.imap(Feature.fromString)(_.toString), false)
    }
}

final class MovieSpec extends DatabaseSuite with SqlMovieSpec {
  lazy val mapping = MovieMapping.mkMapping(pool)
}
