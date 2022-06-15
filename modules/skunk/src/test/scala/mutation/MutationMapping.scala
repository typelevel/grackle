// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package mutation

import cats.effect.{Resource, Sync}
import cats.syntax.all._
import skunk.Session
import skunk.codec.{ all => codec }
import skunk.implicits._

import edu.gemini.grackle._
import skunk.{SkunkMappingCompanion, SkunkMonitor}

import utils.{DatabaseSuite, SkunkTestMapping}
import grackle.test.SqlMutationSpec

object MutationMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkTestMapping[F](pool, monitor) with SqlMutationMapping[F] {
      def updatePopulation(id: Int, population: Int): F[Unit] =
        pool.use { s =>
          s.prepare(sql"update city set population=${codec.int4} where id=${codec.int4}".command).use { ps =>
            ps.execute(population ~ id).void
          }
        }

      def createCity(name: String, countryCode: String, population: Int): F[Int] =
        pool.use { s =>
          val q = sql"""
              INSERT INTO city (id, name, countrycode, district, population)
              VALUES (nextval('city_id'), ${codec.varchar}, ${codec.bpchar(3)}, 'ignored', ${codec.int4})
              RETURNING id
            """.query(codec.int4)
          s.prepare(q).use { ps =>
            ps.unique(name ~ countryCode ~ population)
          }
        }
    }
}

final class MutationSpec extends DatabaseSuite with SqlMutationSpec {
  lazy val mapping = MutationMapping.mkMapping(pool)
}
