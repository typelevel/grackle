// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package grackle.doobie.test

import cats.effect.IO
import doobie.implicits._
import doobie.Meta

import grackle.doobie.postgres.DoobieMonitor
import grackle.sql.SqlStatsMonitor

import grackle.sql.test._
import grackle.Mapping

final class ArrayJoinSuite extends DoobieDatabaseSuite with SqlArrayJoinSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlArrayJoinMapping[IO]
}

final class CoalesceSuite extends DoobieDatabaseSuite with SqlCoalesceSuite {
  type Fragment = doobie.Fragment
  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO,Fragment])] =
    DoobieMonitor.statsMonitor[IO].map(mon => (new DoobieTestMapping(xa, mon) with SqlCoalesceMapping[IO], mon))
}

final class ComposedWorldSuite extends DoobieDatabaseSuite with SqlComposedWorldSuite {
  def mapping: IO[(CurrencyMapping[IO], Mapping[IO])] =
    for {
      currencyMapping <- CurrencyMapping[IO]
    } yield (currencyMapping, new SqlComposedMapping(new DoobieTestMapping(xa) with SqlWorldMapping[IO], currencyMapping))
}

final class CompositeKeySuite extends DoobieDatabaseSuite with SqlCompositeKeySuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlCompositeKeyMapping[IO]
}

final class CursorJsonSuite extends DoobieDatabaseSuite with SqlCursorJsonSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlCursorJsonMapping[IO]
}

final class EmbeddingSuite extends DoobieDatabaseSuite with SqlEmbeddingSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlEmbeddingMapping[IO]
}

final class Embedding2Suite extends DoobieDatabaseSuite with SqlEmbedding2Suite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlEmbedding2Mapping[IO]
}

final class Embedding3Suite extends DoobieDatabaseSuite with SqlEmbedding3Suite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlEmbedding3Mapping[IO]
}

final class FilterJoinAliasSuite extends DoobieDatabaseSuite with SqlFilterJoinAliasSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlFilterJoinAliasMapping[IO]
}

final class FilterOrderOffsetLimitSuite extends DoobieDatabaseSuite with SqlFilterOrderOffsetLimitSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlFilterOrderOffsetLimitMapping[IO]
}

final class FilterOrderOffsetLimit2Suite extends DoobieDatabaseSuite with SqlFilterOrderOffsetLimit2Suite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlFilterOrderOffsetLimit2Mapping[IO]
}

final class GraphSuite extends DoobieDatabaseSuite with SqlGraphSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlGraphMapping[IO]
}

final class InterfacesSuite extends DoobieDatabaseSuite with SqlInterfacesSuite {
  lazy val mapping =
    new DoobieTestMapping(xa) with SqlInterfacesMapping[IO] {
      def entityType: Codec =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class InterfacesSuite2 extends DoobieDatabaseSuite with SqlInterfacesSuite2 {
  lazy val mapping =
    new DoobieTestMapping(xa) with SqlInterfacesMapping2[IO] {
      def entityType: Codec =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class JsonbSuite extends DoobieDatabaseSuite with SqlJsonbSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlJsonbMapping[IO]
}

final class LikeSuite extends DoobieDatabaseSuite with SqlLikeSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlLikeMapping[IO]
}

final class MixedSuite extends DoobieDatabaseSuite with SqlMixedSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlMixedMapping[IO]
}

final class MovieSuite extends DoobieDatabaseSuite with SqlMovieSuite {
  lazy val mapping =
    new DoobieTestMapping(xa) with SqlMovieMapping[IO] {
      def genre: Codec = (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)
      def feature: Codec = (Meta[String].imap(Feature.fromString)(_.toString), false)
    }
}

final class MutationSuite extends DoobieDatabaseSuite with SqlMutationSuite {
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

final class NestedEffectsSuite extends DoobieDatabaseSuite with SqlNestedEffectsSuite {
  def mapping: IO[(CurrencyService[IO], Mapping[IO])] =
    for {
      currencyService0 <- CurrencyService[IO]
    } yield {
      val mapping =
        new DoobieTestMapping(xa) with SqlNestedEffectsMapping[IO] {
          lazy val currencyService = currencyService0
        }
      (currencyService0, mapping)
    }
}

final class Paging1Suite extends DoobieDatabaseSuite with SqlPaging1Suite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlPaging1Mapping[IO]
}

final class Paging2Suite extends DoobieDatabaseSuite with SqlPaging2Suite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlPaging2Mapping[IO]
}

final class Paging3Suite extends DoobieDatabaseSuite with SqlPaging3Suite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlPaging3Mapping[IO]
}

final class ProjectionSuite extends DoobieDatabaseSuite with SqlProjectionSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlProjectionMapping[IO]
}

final class RecursiveInterfacesSuite extends DoobieDatabaseSuite with SqlRecursiveInterfacesSuite {
  lazy val mapping =
    new DoobieTestMapping(xa) with SqlRecursiveInterfacesMapping[IO] {
      def itemType: Codec =
        (Meta[Int].timap(ItemType.fromInt)(ItemType.toInt), false)
    }
}

final class SiblingListsSuite extends DoobieDatabaseSuite with SqlSiblingListsSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlSiblingListsData[IO]
}

final class TreeSuite extends DoobieDatabaseSuite with SqlTreeSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlTreeMapping[IO]
}

final class UnionsSuite extends DoobieDatabaseSuite with SqlUnionSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlUnionsMapping[IO]
}

final class WorldSuite extends DoobieDatabaseSuite with SqlWorldSuite {
  lazy val mapping = new DoobieTestMapping(xa) with SqlWorldMapping[IO]
}

final class WorldCompilerSuite extends DoobieDatabaseSuite with SqlWorldCompilerSuite {
  type Fragment = doobie.Fragment

  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO,Fragment])] =
    DoobieMonitor.statsMonitor[IO].map(mon => (new DoobieTestMapping(xa, mon) with SqlWorldMapping[IO], mon))

  def simpleRestrictedQuerySql: String =
    "SELECT country.code , country.name FROM country WHERE ( country.code = ?)"

  def simpleFilteredQuerySql: String =
    "SELECT city.id , city.name FROM city WHERE city.name ILIKE ?"
}
