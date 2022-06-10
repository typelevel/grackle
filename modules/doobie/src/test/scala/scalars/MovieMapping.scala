// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package scalars

import cats.effect.Sync
import doobie.Transactor
import doobie.util.meta.Meta

import edu.gemini.grackle.Mapping
import edu.gemini.grackle.doobie.{DoobieMappingCompanion, DoobieMonitor}
import grackle.test.SqlMovieSpec
import utils.{DatabaseSuite, DoobieTestMapping}

object MovieMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping[F](transactor, monitor) with SqlMovieMapping[F] {
      def genre: Codec =
        (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)

      def feature: Codec =
        (Meta[String].imap(Feature.fromString)(_.toString), false)
    }
}

final class MovieSpec extends DatabaseSuite with SqlMovieSpec {
  lazy val mapping = MovieMapping.mkMapping(xa)
}
