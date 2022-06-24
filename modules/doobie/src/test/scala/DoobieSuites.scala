// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import cats.effect.IO
import doobie.implicits._
import doobie.Meta
import io.circe.Json

import edu.gemini.grackle.QueryExecutor
import edu.gemini.grackle.doobie.DoobieMonitor
import edu.gemini.grackle.sql.SqlStatsMonitor

final class ArrayJoinSpec extends DoobieDatabaseSuite with SqlArrayJoinSpec {
  lazy val mapping = new DoobieTestMapping(xa) with SqlArrayJoinMapping[IO]
}

final class CoalesceSpec extends DoobieDatabaseSuite with SqlCoalesceSpec {
  type Fragment = doobie.Fragment
  def mapping: IO[(QueryExecutor[IO, Json], SqlStatsMonitor[IO,Fragment])] =
    DoobieMonitor.statsMonitor[IO].map(mon => (new DoobieTestMapping(xa, mon) with SqlCoalesceMapping[IO], mon))
}

final class ComposedWorldSpec extends DoobieDatabaseSuite with SqlComposedWorldSpec {
  lazy val mapping = new SqlComposedMapping(new DoobieTestMapping(xa) with SqlWorldMapping[IO], CurrencyMapping[IO])
}

final class CursorJsonSpec extends DoobieDatabaseSuite with SqlCursorJsonSpec {
  lazy val mapping = new DoobieTestMapping(xa) with SqlCursorJsonMapping[IO]
}

final class EmbeddingSpec extends DoobieDatabaseSuite with SqlEmbeddingSpec {
  lazy val mapping = new DoobieTestMapping(xa) with SqlEmbeddingMapping[IO]
}

final class FilterOrderOffsetLimitSpec extends DoobieDatabaseSuite with SqlFilterOrderOffsetLimitSpec {
  lazy val mapping = new DoobieTestMapping(xa) with SqlFilterOrderOffsetLimitMapping[IO]
}

final class FilterOrderOffsetLimit2Spec extends DoobieDatabaseSuite with SqlFilterOrderOffsetLimit2Spec {
  lazy val mapping = new DoobieTestMapping(xa) with SqlFilterOrderOffsetLimit2Mapping[IO]
}

final class GraphSpec extends DoobieDatabaseSuite with SqlGraphSpec {
  lazy val mapping = new DoobieTestMapping(xa) with SqlGraphMapping[IO]
}

final class InterfacesSpec extends DoobieDatabaseSuite with SqlInterfacesSpec {
  lazy val mapping = 
    new DoobieTestMapping(xa) with SqlInterfacesMapping[IO] {
      def entityType: Codec =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class JsonbSpec extends DoobieDatabaseSuite with SqlJsonbSpec {
  lazy val mapping = new DoobieTestMapping(xa) with SqlJsonbMapping[IO]
}

final class MovieSpec extends DoobieDatabaseSuite with SqlMovieSpec {
  lazy val mapping = 
    new DoobieTestMapping(xa) with SqlMovieMapping[IO] {
      def genre: Codec = (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)
      def feature: Codec = (Meta[String].imap(Feature.fromString)(_.toString), false)
    }
}

final class MutationSpec extends DoobieDatabaseSuite with SqlMutationSpec {
  lazy val mapping = 
    new DoobieTestMapping(xa) with SqlMutationMapping[IO] {
      def updatePopulation(id: Int, population: Int): IO[Unit] =
        sql"update city set population=$population where id=$id"
          .update
          .run
          .transact(transactor)
          .as(())

      def createCity(name: String, countryCode: String, population: Int): IO[Int] =
        sql"""
          INSERT INTO city (id, name, countrycode, district, population)
          VALUES (nextval('city_id'), $name, $countryCode, 'ignored', $population)
          RETURNING id
          """.query[Int]
            .unique
            .transact(transactor)
    }
}

final class ProjectionSpec extends DoobieDatabaseSuite with SqlProjectionSpec {
  lazy val mapping = new DoobieTestMapping(xa) with SqlProjectionMapping[IO]
}

final class RecursiveInterfacesSpec extends DoobieDatabaseSuite with SqlRecursiveInterfacesSpec {
  lazy val mapping = 
    new DoobieTestMapping(xa) with SqlRecursiveInterfacesMapping[IO] {
      def itemType: Codec =
        (Meta[Int].timap(ItemType.fromInt)(ItemType.toInt), false)
    }
}

final class SiblingListsSpec extends DoobieDatabaseSuite with SqlSiblingListsSpec {
  lazy val mapping = new DoobieTestMapping(xa) with SqlSiblingListsData[IO]
}

final class TreeSpec extends DoobieDatabaseSuite with SqlTreeSpec {
  lazy val mapping = new DoobieTestMapping(xa) with SqlTreeMapping[IO]
}

final class UnionsSpec extends DoobieDatabaseSuite with SqlUnionSpec {
  lazy val mapping = new DoobieTestMapping(xa) with SqlUnionsMapping[IO]
}

final class WorldSpec extends DoobieDatabaseSuite with SqlWorldSpec {
  lazy val mapping = new DoobieTestMapping(xa) with SqlWorldMapping[IO]
}

final class WorldCompilerSpec extends DoobieDatabaseSuite with SqlWorldCompilerSpec {
  type Fragment = doobie.Fragment

  def mapping: IO[(QueryExecutor[IO, Json], SqlStatsMonitor[IO,Fragment])] =
    DoobieMonitor.statsMonitor[IO].map(mon => (new DoobieTestMapping(xa, mon) with SqlWorldMapping[IO], mon))

  def simpleRestrictedQuerySql: String =
    "SELECT country.code , country.name FROM country WHERE (country.code = ?)"

  def simpleFilteredQuerySql: String =
    "SELECT city.id , city.name FROM city WHERE (city.name ILIKE ?)"
}
