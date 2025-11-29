// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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

package grackle.doobie.mssql
package test

import cats.effect.{IO, Resource}
import doobie.{Meta, Transactor}
import doobie.implicits._
import munit.catseffect.IOFixture

import grackle.doobie.DoobieMonitor
import grackle.sql.SqlStatsMonitor

import grackle.Mapping
import grackle.sql.test._

final class ArrayJoinSuite extends DoobieMSSqlDatabaseSuite with SqlArrayJoinSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlArrayJoinMapping[IO]
}

final class CoalesceSuite extends DoobieMSSqlDatabaseSuite with SqlCoalesceSuite {
  type Fragment = doobie.Fragment
  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO,Fragment])] =
    DoobieMonitor.statsMonitor[IO].map(mon => (new DoobieMSSqlTestMapping(transactor, mon) with SqlCoalesceMapping[IO], mon))
}

final class ComposedWorldSuite extends DoobieMSSqlDatabaseSuite with SqlComposedWorldSuite {
  def mapping: IO[(CurrencyMapping[IO], Mapping[IO])] =
    for {
      currencyMapping <- CurrencyMapping[IO]
    } yield (currencyMapping, new SqlComposedMapping(new DoobieMSSqlTestMapping(transactor) with SqlWorldMapping[IO], currencyMapping))
}

final class CompositeKeySuite extends DoobieMSSqlDatabaseSuite with SqlCompositeKeySuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlCompositeKeyMapping[IO]
}

final class CursorJsonSuite extends DoobieMSSqlDatabaseSuite with SqlCursorJsonSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlCursorJsonMapping[IO]
}

final class EmbeddingSuite extends DoobieMSSqlDatabaseSuite with SqlEmbeddingSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlEmbeddingMapping[IO]
}

final class Embedding2Suite extends DoobieMSSqlDatabaseSuite with SqlEmbedding2Suite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlEmbedding2Mapping[IO]
}

final class Embedding3Suite extends DoobieMSSqlDatabaseSuite with SqlEmbedding3Suite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlEmbedding3Mapping[IO]
}

final class FilterJoinAliasSuite extends DoobieMSSqlDatabaseSuite with SqlFilterJoinAliasSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlFilterJoinAliasMapping[IO]
}

final class FilterOrderOffsetLimitSuite extends DoobieMSSqlDatabaseSuite with SqlFilterOrderOffsetLimitSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlFilterOrderOffsetLimitMapping[IO]
}

final class FilterOrderOffsetLimit2Suite extends DoobieMSSqlDatabaseSuite with SqlFilterOrderOffsetLimit2Suite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlFilterOrderOffsetLimit2Mapping[IO]
}

final class GraphSuite extends DoobieMSSqlDatabaseSuite with SqlGraphSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlGraphMapping[IO]
}

final class InterfacesSuite extends DoobieMSSqlDatabaseSuite with SqlInterfacesSuite {
  lazy val mapping =
    new DoobieMSSqlTestMapping(transactor) with SqlInterfacesMapping[IO] {
      def entityType: TestCodec[EntityType] =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class InterfacesSuite2 extends DoobieMSSqlDatabaseSuite with SqlInterfacesSuite2 {
  lazy val mapping =
    new DoobieMSSqlTestMapping(transactor) with SqlInterfacesMapping2[IO] {
      def entityType: TestCodec[EntityType] =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class JsonbSuite extends DoobieMSSqlDatabaseSuite with SqlJsonbSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlJsonbMapping[IO]
}

final class LikeSuite extends DoobieMSSqlDatabaseSuite with SqlLikeSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlLikeMapping[IO]
}

final class MappingValidatorValidSuite extends DoobieMSSqlDatabaseSuite with SqlMappingValidatorValidSuite {
  // no DB instance needed for this suite
  lazy val mapping = new DoobieMSSqlTestMapping(null) with SqlMappingValidatorValidMapping[IO] {
    def genre: TestCodec[Genre] = (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)
    def feature: TestCodec[Feature] = (Meta[String].imap(Feature.fromString)(_.toString), false)
  }
  override def munitFixtures: Seq[IOFixture[_]] = Nil
}

final class MappingValidatorInvalidSuite extends DoobieMSSqlDatabaseSuite with SqlMappingValidatorInvalidSuite {
  // no DB instance needed for this suite
  lazy val mapping = new DoobieMSSqlTestMapping(null) with SqlMappingValidatorInvalidMapping[IO]
  override def munitFixtures: Seq[IOFixture[_]] = Nil
}

final class MixedSuite extends DoobieMSSqlDatabaseSuite with SqlMixedSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlMixedMapping[IO]
}

final class MovieSuite extends DoobieMSSqlDatabaseSuite with SqlMovieSuite {
  lazy val mapping =
    new DoobieMSSqlTestMapping(transactor) with SqlMovieMapping[IO] {
      def genre: TestCodec[Genre] = (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)
      def feature: TestCodec[Feature] = (Meta[String].imap(Feature.fromString)(_.toString), false)
      def tagList: TestCodec[List[String]] = (Meta[Int].imap(Tags.fromInt)(Tags.toInt), false)
    }
}

final class MutationSuite extends DoobieMSSqlDatabaseSuite with SqlMutationSuite {
  // A resource that copies and drops the table used in the tests.
  def withDuplicatedTables(transactor: Transactor[IO]): Resource[IO, Transactor[IO]] = {
    val alloc = sql"SELECT * INTO city_copy FROM city".update.run.transact(transactor).as(transactor)
    val free  = sql"DROP TABLE city_copy".update.run.transact(transactor).void
    Resource.make(alloc)(_ => free)
  }

  override def transactorResource: Resource[IO, Transactor[IO]] =
    super.transactorResource.flatMap(withDuplicatedTables)

  lazy val mapping =
    new DoobieMSSqlTestMapping(transactor) with SqlMutationMapping[IO] {
      def updatePopulation(id: Int, population: Int): IO[Unit] =
        sql"UPDATE city_copy SET population=$population WHERE id=$id"
          .update
          .run
          .transact(transactor)
          .as(())

      def createCity(name: String, countryCode: String, population: Int): IO[Int] =
        sql"""
          INSERT INTO city_copy (id, name, countrycode, district, population)
          OUTPUT INSERTED.ID
          VALUES (NEXT VALUE FOR city_id, $name, $countryCode, 'ignored', $population)
        """
          .query[Int]
          .unique
          .transact(transactor)
    }
}

final class NestedEffectsSuite extends DoobieMSSqlDatabaseSuite with SqlNestedEffectsSuite {
  def mapping: IO[(CurrencyService[IO], Mapping[IO])] =
    for {
      currencyService0 <- CurrencyService[IO]
    } yield {
      val mapping =
        new DoobieMSSqlTestMapping(transactor) with SqlNestedEffectsMapping[IO] {
          lazy val currencyService = currencyService0
        }
      (currencyService0, mapping)
    }
}

final class Paging1Suite extends DoobieMSSqlDatabaseSuite with SqlPaging1Suite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlPaging1Mapping[IO]
}

final class Paging2Suite extends DoobieMSSqlDatabaseSuite with SqlPaging2Suite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlPaging2Mapping[IO]
}

final class Paging3Suite extends DoobieMSSqlDatabaseSuite with SqlPaging3Suite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlPaging3Mapping[IO]
}

final class ProjectionSuite extends DoobieMSSqlDatabaseSuite with SqlProjectionSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlProjectionMapping[IO]
}

final class RecursiveInterfacesSuite extends DoobieMSSqlDatabaseSuite with SqlRecursiveInterfacesSuite {
  lazy val mapping =
    new DoobieMSSqlTestMapping(transactor) with SqlRecursiveInterfacesMapping[IO] {
      def itemType: TestCodec[ItemType] =
        (Meta[Int].timap(ItemType.fromInt)(ItemType.toInt), false)
    }
}

final class SiblingListsSuite extends DoobieMSSqlDatabaseSuite with SqlSiblingListsSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlSiblingListsData[IO]
}

final class TreeSuite extends DoobieMSSqlDatabaseSuite with SqlTreeSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlTreeMapping[IO]
}

final class UnionsSuite extends DoobieMSSqlDatabaseSuite with SqlUnionSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlUnionsMapping[IO]
}

final class WorldSuite extends DoobieMSSqlDatabaseSuite with SqlWorldSuite {
  lazy val mapping = new DoobieMSSqlTestMapping(transactor) with SqlWorldMapping[IO]
}

final class WorldCompilerSuite extends DoobieMSSqlDatabaseSuite with SqlWorldCompilerSuite {
  type Fragment = doobie.Fragment

  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO,Fragment])] =
    DoobieMonitor.statsMonitor[IO].map(mon => (new DoobieMSSqlTestMapping(transactor, mon) with SqlWorldMapping[IO], mon))

  def simpleRestrictedQuerySql: String =
    "SELECT country.code , country.name FROM country WHERE (( country.code = ?) )"

  def simpleFilteredQuerySql: String =
    "SELECT city.id , city.name FROM city WHERE (UPPER( city.name ) LIKE ?)"

  def filterArg: String = "LINH%"
}
