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

package grackle.doobie.postgres
package test

import cats.effect.{IO, Resource}
import doobie.{Meta, Transactor}
import doobie.implicits._
import munit.catseffect.IOFixture

import grackle.doobie.DoobieMonitor
import grackle.sql.SqlStatsMonitor

import grackle.Mapping
import grackle.sql.test._

final class ArrayJoinSuite extends DoobiePgDatabaseSuite with SqlArrayJoinSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlArrayJoinMapping[IO]
}

final class CoalesceSuite extends DoobiePgDatabaseSuite with SqlCoalesceSuite {
  type Fragment = doobie.Fragment
  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO,Fragment])] =
    DoobieMonitor.statsMonitor[IO].map(mon => (new DoobiePgTestMapping(transactor, mon) with SqlCoalesceMapping[IO], mon))
}

final class ComposedWorldSuite extends DoobiePgDatabaseSuite with SqlComposedWorldSuite {
  def mapping: IO[(CurrencyMapping[IO], Mapping[IO])] =
    for {
      currencyMapping <- CurrencyMapping[IO]
    } yield (currencyMapping, new SqlComposedMapping(new DoobiePgTestMapping(transactor) with SqlWorldMapping[IO], currencyMapping))
}

final class CompositeKeySuite extends DoobiePgDatabaseSuite with SqlCompositeKeySuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlCompositeKeyMapping[IO]
}

final class CursorJsonSuite extends DoobiePgDatabaseSuite with SqlCursorJsonSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlCursorJsonMapping[IO]
}

final class EmbeddingSuite extends DoobiePgDatabaseSuite with SqlEmbeddingSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlEmbeddingMapping[IO]
}

final class Embedding2Suite extends DoobiePgDatabaseSuite with SqlEmbedding2Suite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlEmbedding2Mapping[IO]
}

final class Embedding3Suite extends DoobiePgDatabaseSuite with SqlEmbedding3Suite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlEmbedding3Mapping[IO]
}

final class FilterJoinAliasSuite extends DoobiePgDatabaseSuite with SqlFilterJoinAliasSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlFilterJoinAliasMapping[IO]
}

final class FilterOrderOffsetLimitSuite extends DoobiePgDatabaseSuite with SqlFilterOrderOffsetLimitSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlFilterOrderOffsetLimitMapping[IO]
}

final class FilterOrderOffsetLimit2Suite extends DoobiePgDatabaseSuite with SqlFilterOrderOffsetLimit2Suite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlFilterOrderOffsetLimit2Mapping[IO]
}

final class GraphSuite extends DoobiePgDatabaseSuite with SqlGraphSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlGraphMapping[IO]
}

final class InterfacesSuite extends DoobiePgDatabaseSuite with SqlInterfacesSuite {
  lazy val mapping =
    new DoobiePgTestMapping(transactor) with SqlInterfacesMapping[IO] {
      def entityType: TestCodec[EntityType] =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class InterfacesSuite2 extends DoobiePgDatabaseSuite with SqlInterfacesSuite2 {
  lazy val mapping =
    new DoobiePgTestMapping(transactor) with SqlInterfacesMapping2[IO] {
      def entityType: TestCodec[EntityType] =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class JsonbSuite extends DoobiePgDatabaseSuite with SqlJsonbSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlJsonbMapping[IO]
}

final class LikeSuite extends DoobiePgDatabaseSuite with SqlLikeSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlLikeMapping[IO]
}

final class MappingValidatorValidSuite extends DoobiePgDatabaseSuite with SqlMappingValidatorValidSuite {
  // no DB instance needed for this suite
  lazy val mapping = new DoobiePgTestMapping(null) with SqlMappingValidatorValidMapping[IO] {
    def genre: TestCodec[Genre] = (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)
    def feature: TestCodec[Feature] = (Meta[String].imap(Feature.fromString)(_.toString), false)
  }
  override def munitFixtures: Seq[IOFixture[_]] = Nil
}

final class MappingValidatorInvalidSuite extends DoobiePgDatabaseSuite with SqlMappingValidatorInvalidSuite {
  // no DB instance needed for this suite
  lazy val mapping = new DoobiePgTestMapping(null) with SqlMappingValidatorInvalidMapping[IO]
  override def munitFixtures: Seq[IOFixture[_]] = Nil
}

final class MixedSuite extends DoobiePgDatabaseSuite with SqlMixedSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlMixedMapping[IO]
}

final class MovieSuite extends DoobiePgDatabaseSuite with SqlMovieSuite {
  lazy val mapping =
    new DoobiePgTestMapping(transactor) with SqlMovieMapping[IO] {
      def genre: TestCodec[Genre] = (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)
      def feature: TestCodec[Feature] = (Meta[String].imap(Feature.fromString)(_.toString), false)
      def tagList: TestCodec[List[String]] = (Meta[Int].imap(Tags.fromInt)(Tags.toInt), false)
    }
}

final class MutationSuite extends DoobiePgDatabaseSuite with SqlMutationSuite {
  // A resource that copies and drops the table used in the tests.
  def withDuplicatedTables(transactor: Transactor[IO]): Resource[IO, Transactor[IO]] = {
    val alloc = sql"CREATE TABLE city_copy AS SELECT * FROM city".update.run.transact(transactor).as(transactor)
    val free  = sql"DROP TABLE city_copy".update.run.transact(transactor).void
    Resource.make(alloc)(_ => free)
  }

  override def transactorResource: Resource[IO, Transactor[IO]] =
    super.transactorResource.flatMap(withDuplicatedTables)

  lazy val mapping =
    new DoobiePgTestMapping(transactor) with SqlMutationMapping[IO] {
      def updatePopulation(id: Int, population: Int): IO[Unit] =
        sql"UPDATE city_copy SET population=$population WHERE id=$id"
          .update
          .run
          .transact(transactor)
          .void

      def createCity(name: String, countryCode: String, population: Int): IO[Int] =
        sql"""
          INSERT INTO city_copy (id, name, countrycode, district, population)
          VALUES (nextval('city_id'), $name, $countryCode, 'ignored', $population)
          RETURNING id
          """.query[Int]
            .unique
            .transact(transactor)
    }
}

final class NestedEffectsSuite extends DoobiePgDatabaseSuite with SqlNestedEffectsSuite {
  def mapping: IO[(CurrencyService[IO], Mapping[IO])] =
    for {
      currencyService0 <- CurrencyService[IO]
    } yield {
      val mapping =
        new DoobiePgTestMapping(transactor) with SqlNestedEffectsMapping[IO] {
          lazy val currencyService = currencyService0
        }
      (currencyService0, mapping)
    }
}

final class Paging1Suite extends DoobiePgDatabaseSuite with SqlPaging1Suite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlPaging1Mapping[IO]
}

final class Paging2Suite extends DoobiePgDatabaseSuite with SqlPaging2Suite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlPaging2Mapping[IO]
}

final class Paging3Suite extends DoobiePgDatabaseSuite with SqlPaging3Suite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlPaging3Mapping[IO]
}

final class ProjectionSuite extends DoobiePgDatabaseSuite with SqlProjectionSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlProjectionMapping[IO]
}

final class RecursiveInterfacesSuite extends DoobiePgDatabaseSuite with SqlRecursiveInterfacesSuite {
  lazy val mapping =
    new DoobiePgTestMapping(transactor) with SqlRecursiveInterfacesMapping[IO] {
      def itemType: TestCodec[ItemType] =
        (Meta[Int].timap(ItemType.fromInt)(ItemType.toInt), false)
    }
}

final class SiblingListsSuite extends DoobiePgDatabaseSuite with SqlSiblingListsSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlSiblingListsData[IO]
}

final class TreeSuite extends DoobiePgDatabaseSuite with SqlTreeSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlTreeMapping[IO]
}

final class UnionsSuite extends DoobiePgDatabaseSuite with SqlUnionSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlUnionsMapping[IO]
}

final class WorldSuite extends DoobiePgDatabaseSuite with SqlWorldSuite {
  lazy val mapping = new DoobiePgTestMapping(transactor) with SqlWorldMapping[IO]
}

final class WorldCompilerSuite extends DoobiePgDatabaseSuite with SqlWorldCompilerSuite {
  type Fragment = doobie.Fragment

  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO,Fragment])] =
    DoobieMonitor.statsMonitor[IO].map(mon => (new DoobiePgTestMapping(transactor, mon) with SqlWorldMapping[IO], mon))

  def simpleRestrictedQuerySql: String =
    "SELECT country.code , country.name FROM country WHERE (( country.code = ?) )"

  def simpleFilteredQuerySql: String =
    "SELECT city.id , city.name FROM city WHERE (city.name ILIKE ?)"

  def filterArg: String = "Linh%"
}
