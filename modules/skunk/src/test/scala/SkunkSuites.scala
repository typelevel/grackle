// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import cats.effect.IO
import skunk.codec.{all => codec}
import skunk.implicits._
import io.circe.Json

import edu.gemini.grackle.QueryExecutor
import edu.gemini.grackle.skunk.SkunkMonitor
import edu.gemini.grackle.sql.SqlStatsMonitor

final class ArrayJoinSpec extends SkunkDatabaseSuite with SqlArrayJoinSpec {
  lazy val mapping = new SkunkTestMapping(pool) with SqlArrayJoinMapping[IO]
}

final class CoalesceSpec extends SkunkDatabaseSuite with SqlCoalesceSpec {
  type Fragment = skunk.AppliedFragment
  def mapping: IO[(QueryExecutor[IO, Json], SqlStatsMonitor[IO,Fragment])] =
    SkunkMonitor.statsMonitor[IO].map(mon => (new SkunkTestMapping(pool, mon) with SqlCoalesceMapping[IO], mon))
}

final class ComposedWorldSpec extends SkunkDatabaseSuite with SqlComposedWorldSpec {
  lazy val mapping = new SqlComposedMapping(new SkunkTestMapping(pool) with SqlWorldMapping[IO], CurrencyMapping[IO])
}

final class CursorJsonSpec extends SkunkDatabaseSuite with SqlCursorJsonSpec {
  lazy val mapping = new SkunkTestMapping(pool) with SqlCursorJsonMapping[IO]
}

final class EmbeddingSpec extends SkunkDatabaseSuite with SqlEmbeddingSpec {
  lazy val mapping = new SkunkTestMapping(pool) with SqlEmbeddingMapping[IO]
}

final class FilterOrderOffsetLimitSpec extends SkunkDatabaseSuite with SqlFilterOrderOffsetLimitSpec {
  lazy val mapping = new SkunkTestMapping(pool) with SqlFilterOrderOffsetLimitMapping[IO]
}

final class FilterOrderOffsetLimit2Spec extends SkunkDatabaseSuite with SqlFilterOrderOffsetLimit2Spec {
  lazy val mapping = new SkunkTestMapping(pool) with SqlFilterOrderOffsetLimit2Mapping[IO]
}

final class GraphSpec extends SkunkDatabaseSuite with SqlGraphSpec {
  lazy val mapping = new SkunkTestMapping(pool) with SqlGraphMapping[IO]
}

final class InterfacesSpec extends SkunkDatabaseSuite with SqlInterfacesSpec {
  lazy val mapping = 
    new SkunkTestMapping(pool) with SqlInterfacesMapping[IO] {
      def entityType: Codec =
        (codec.int4.imap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class JsonbSpec extends SkunkDatabaseSuite with SqlJsonbSpec {
  lazy val mapping = new SkunkTestMapping(pool) with SqlJsonbMapping[IO]
}

final class MovieSpec extends SkunkDatabaseSuite with SqlMovieSpec {
  lazy val mapping = 
    new SkunkTestMapping(pool) with SqlMovieMapping[IO] {
      def genre: Codec = (codec.int4.imap(Genre.fromInt)(Genre.toInt), false)
      def feature: Codec = (codec.varchar.imap(Feature.fromString)(_.toString), false)
    }
}

final class MutationSpec extends SkunkDatabaseSuite with SqlMutationSpec {
  lazy val mapping = 
    new SkunkTestMapping(pool) with SqlMutationMapping[IO] {
      def updatePopulation(id: Int, population: Int): IO[Unit] =
        pool.use { s =>
          s.prepare(sql"update city set population=${codec.int4} where id=${codec.int4}".command).use { ps =>
            ps.execute(population ~ id).void
          }
        }

      def createCity(name: String, countryCode: String, population: Int): IO[Int] =
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

final class ProjectionSpec extends SkunkDatabaseSuite with SqlProjectionSpec {
  lazy val mapping = new SkunkTestMapping(pool) with SqlProjectionMapping[IO]
}

final class RecursiveInterfacesSpec extends SkunkDatabaseSuite with SqlRecursiveInterfacesSpec {
  lazy val mapping = 
    new SkunkTestMapping(pool) with SqlRecursiveInterfacesMapping[IO] {
      def itemType: Codec =
        (codec.int4.imap(ItemType.fromInt)(ItemType.toInt), false)
    }
}

final class SiblingListsSpec extends SkunkDatabaseSuite with SqlSiblingListsSpec {
  lazy val mapping = new SkunkTestMapping(pool) with SqlSiblingListsData[IO]
}

final class TreeSpec extends SkunkDatabaseSuite with SqlTreeSpec {
  lazy val mapping = new SkunkTestMapping(pool) with SqlTreeMapping[IO]
}

final class UnionsSpec extends SkunkDatabaseSuite with SqlUnionSpec {
  lazy val mapping = new SkunkTestMapping(pool) with SqlUnionsMapping[IO]
}

final class WorldSpec extends SkunkDatabaseSuite with SqlWorldSpec {
  lazy val mapping = new SkunkTestMapping(pool) with SqlWorldMapping[IO]
}

final class WorldCompilerSpec extends SkunkDatabaseSuite with SqlWorldCompilerSpec {
  type Fragment = skunk.AppliedFragment

  def mapping: IO[(QueryExecutor[IO, Json], SqlStatsMonitor[IO,Fragment])] =
    SkunkMonitor.statsMonitor[IO].map(mon => (new SkunkTestMapping(pool, mon) with SqlWorldMapping[IO], mon))

  def simpleRestrictedQuerySql: String =
    "SELECT country.code, country.name FROM country WHERE (country.code = $1)"

  def simpleFilteredQuerySql: String =
    "SELECT city.id, city.name FROM city WHERE (city.name ILIKE $1)"
}
