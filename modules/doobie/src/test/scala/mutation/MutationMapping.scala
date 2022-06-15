// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package mutation

import cats.effect.Sync
import cats.syntax.all._
import doobie.Transactor
import doobie.implicits._

import edu.gemini.grackle._
import doobie.{DoobieMappingCompanion, DoobieMonitor}

import utils.{DatabaseSuite, DoobieTestMapping}
import grackle.test.SqlMutationSpec

object MutationMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieTestMapping[F](transactor, monitor) with SqlMutationMapping[F] {
      def updatePopulation(id: Int, population: Int): F[Unit] =
        sql"update city set population=$population where id=$id"
          .update
          .run
          .transact(transactor)
          .as(())

      def createCity(name: String, countryCode: String, population: Int): F[Int] =
        sql"""
          INSERT INTO city (id, name, countrycode, district, population)
          VALUES (nextval('city_id'), $name, $countryCode, 'ignored', $population)
          RETURNING id
          """.query[Int]
            .unique
            .transact(transactor)
    }
}

final class MutationSpec extends DatabaseSuite with SqlMutationSpec {
  lazy val mapping = MutationMapping.mkMapping(xa)
}
