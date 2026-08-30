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

package grackle.doobie.h2.test

// Every shared sql-core suite is wired up here, matching the doobie-pg/doobie-oracle/
// doobie-mssql/doobie-sqlite suites this is modelled on.

import cats.effect.{IO, Resource}
import munit.catseffect.IOFixture
import org.typelevel.doobie.{Meta, Transactor}
import org.typelevel.doobie.implicits._

import grackle.Mapping
import grackle.doobie.DoobieMonitor
import grackle.sql.SqlStatsMonitor
import grackle.sql.test._

final class ArrayJoinSuite extends DoobieH2DatabaseSuite with SqlArrayJoinSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlArrayJoinMapping[IO]
}

final class CoalesceSuite extends DoobieH2DatabaseSuite with SqlCoalesceSuite {
  type Fragment = org.typelevel.doobie.Fragment
  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO, Fragment])] =
    DoobieMonitor
      .statsMonitor[IO]
      .map(mon => (new DoobieH2TestMapping(transactor, mon) with SqlCoalesceMapping[IO], mon))
}

final class ComposedWorldSuite extends DoobieH2DatabaseSuite with SqlComposedWorldSuite {
  def mapping: IO[(CurrencyMapping[IO], Mapping[IO])] =
    for {
      currencyMapping <- CurrencyMapping[IO]
    } yield (
      currencyMapping,
      new SqlComposedMapping(
        new DoobieH2TestMapping(transactor) with SqlWorldMapping[IO],
        currencyMapping))
}

final class CompositeKeySuite extends DoobieH2DatabaseSuite with SqlCompositeKeySuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlCompositeKeyMapping[IO]
}

final class ErrorKeysSuite extends DoobieH2DatabaseSuite with SqlErrorKeysSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlCompositeKeyMapping[IO]
}

final class CursorJsonSuite extends DoobieH2DatabaseSuite with SqlCursorJsonSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlCursorJsonMapping[IO]
}

final class EmbeddingSuite extends DoobieH2DatabaseSuite with SqlEmbeddingSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlEmbeddingMapping[IO]
}

final class Embedding2Suite extends DoobieH2DatabaseSuite with SqlEmbedding2Suite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlEmbedding2Mapping[IO]
}

final class Embedding3Suite extends DoobieH2DatabaseSuite with SqlEmbedding3Suite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlEmbedding3Mapping[IO]
}

final class FilterJoinAliasSuite extends DoobieH2DatabaseSuite with SqlFilterJoinAliasSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlFilterJoinAliasMapping[IO]
}

final class FilterOrderOffsetLimitSuite
    extends DoobieH2DatabaseSuite
    with SqlFilterOrderOffsetLimitSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor)
    with SqlFilterOrderOffsetLimitMapping[IO]
}

final class FilterOrderOffsetLimit2Suite
    extends DoobieH2DatabaseSuite
    with SqlFilterOrderOffsetLimit2Suite {
  lazy val mapping = new DoobieH2TestMapping(transactor)
    with SqlFilterOrderOffsetLimit2Mapping[IO]
}

final class GraphSuite extends DoobieH2DatabaseSuite with SqlGraphSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlGraphMapping[IO]
}

final class InterfacesSuite extends DoobieH2DatabaseSuite with SqlInterfacesSuite {
  lazy val mapping =
    new DoobieH2TestMapping(transactor) with SqlInterfacesMapping[IO] {
      def entityType: TestCodec[EntityType] =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class InterfacesSuite2 extends DoobieH2DatabaseSuite with SqlInterfacesSuite2 {
  lazy val mapping =
    new DoobieH2TestMapping(transactor) with SqlInterfacesMapping2[IO] {
      def entityType: TestCodec[EntityType] =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class JsonbSuite extends DoobieH2DatabaseSuite with SqlJsonbSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlJsonbMapping[IO]
}

final class LikeSuite extends DoobieH2DatabaseSuite with SqlLikeSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlLikeMapping[IO]
}

final class MappingValidatorValidSuite
    extends DoobieH2DatabaseSuite
    with SqlMappingValidatorValidSuite {
  // no DB instance needed for this suite
  lazy val mapping = new DoobieH2TestMapping(null) with SqlMappingValidatorValidMapping[IO] {
    def genre: TestCodec[Genre] = (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)
    def feature: TestCodec[Feature] = (Meta[String].imap(Feature.fromString)(_.toString), false)
  }
  override def munitFixtures: Seq[IOFixture[_]] = Nil
}

final class MappingValidatorInvalidSuite
    extends DoobieH2DatabaseSuite
    with SqlMappingValidatorInvalidSuite {
  // no DB instance needed for this suite
  lazy val mapping = new DoobieH2TestMapping(null) with SqlMappingValidatorInvalidMapping[IO]
  override def munitFixtures: Seq[IOFixture[_]] = Nil
}

final class MixedSuite extends DoobieH2DatabaseSuite with SqlMixedSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlMixedMapping[IO]
}

final class MovieSuite extends DoobieH2DatabaseSuite with SqlMovieSuite {
  lazy val mapping =
    new DoobieH2TestMapping(transactor) with SqlMovieMapping[IO] {
      def genre: TestCodec[Genre] = (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)
      def feature: TestCodec[Feature] =
        (Meta[String].imap(Feature.fromString)(_.toString), false)
      def tagList: TestCodec[List[String]] = (Meta[Int].imap(Tags.fromInt)(Tags.toInt), false)
    }
}

final class MutationSuite extends DoobieH2DatabaseSuite with SqlMutationSuite {
  // A resource that copies and drops the table used in the tests.
  def withDuplicatedTables(transactor: Transactor[IO]): Resource[IO, Transactor[IO]] = {
    val alloc = sql"CREATE TABLE city_copy AS SELECT * FROM city"
      .update
      .run
      .transact(transactor)
      .as(transactor)
    val free = sql"DROP TABLE city_copy".update.run.transact(transactor).void
    Resource.make(alloc)(_ => free)
  }

  override def transactorResource: Resource[IO, Transactor[IO]] =
    super.transactorResource.flatMap(withDuplicatedTables)

  lazy val mapping =
    new DoobieH2TestMapping(transactor) with SqlMutationMapping[IO] {
      def updatePopulation(id: Int, population: Int): IO[Unit] =
        sql"UPDATE city_copy SET population=$population WHERE id=$id"
          .update
          .run
          .transact(transactor)
          .void

      // H2 has no `INSERT ... RETURNING` in the single-statement form Grackle would want here -
      // its one-statement equivalent is a FINAL TABLE data-change delta table, a different
      // construct - so the id is minted first and inserted explicitly. That's safe here because
      // suites run single-threaded against a private database, so there's no concurrent inserter
      // to race against between the SELECT and the INSERT.
      def createCity(name: String, countryCode: String, population: Int): IO[Int] = {
        val nextId = sql"SELECT COALESCE(MAX(id), 0) + 1 FROM city_copy".query[Int].unique
        def insert(id: Int) =
          sql"""
            INSERT INTO city_copy (id, name, countrycode, district, population)
            VALUES ($id, $name, $countryCode, 'ignored', $population)
            """.update.run
        (for {
          id <- nextId
          _ <- insert(id)
        } yield id).transact(transactor)
      }
    }
}

final class NullableParentSuite extends DoobieH2DatabaseSuite with SqlNullableParentSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlNullableParentMapping[IO]
}

final class NullOrderingSuite extends DoobieH2DatabaseSuite with SqlNullOrderingSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlNullOrderingMapping[IO]
}

final class NestedEffectsSuite extends DoobieH2DatabaseSuite with SqlNestedEffectsSuite {
  def mapping: IO[(CurrencyService[IO], Mapping[IO])] =
    for {
      currencyService0 <- CurrencyService[IO]
    } yield {
      val mapping =
        new DoobieH2TestMapping(transactor) with SqlNestedEffectsMapping[IO] {
          lazy val currencyService = currencyService0
        }
      (currencyService0, mapping)
    }
}

final class Paging1Suite extends DoobieH2DatabaseSuite with SqlPaging1Suite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlPaging1Mapping[IO]
}

final class Paging2Suite extends DoobieH2DatabaseSuite with SqlPaging2Suite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlPaging2Mapping[IO]
}

final class Paging3Suite extends DoobieH2DatabaseSuite with SqlPaging3Suite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlPaging3Mapping[IO]
}

final class ProjectionSuite extends DoobieH2DatabaseSuite with SqlProjectionSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlProjectionMapping[IO]
}

final class RecursiveInterfacesSuite
    extends DoobieH2DatabaseSuite
    with SqlRecursiveInterfacesSuite {
  lazy val mapping =
    new DoobieH2TestMapping(transactor) with SqlRecursiveInterfacesMapping[IO] {
      def itemType: TestCodec[ItemType] =
        (Meta[Int].timap(ItemType.fromInt)(ItemType.toInt), false)
    }
}

final class SiblingListsSuite extends DoobieH2DatabaseSuite with SqlSiblingListsSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlSiblingListsData[IO]
}

final class TreeSuite extends DoobieH2DatabaseSuite with SqlTreeSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlTreeMapping[IO]
}

final class UnionsSuite extends DoobieH2DatabaseSuite with SqlUnionSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlUnionsMapping[IO]
}

final class WorldSuite extends DoobieH2DatabaseSuite with SqlWorldSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with SqlWorldMapping[IO]
}

final class WorldCompilerSuite extends DoobieH2DatabaseSuite with SqlWorldCompilerSuite {
  type Fragment = org.typelevel.doobie.Fragment

  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO, Fragment])] =
    DoobieMonitor
      .statsMonitor[IO]
      .map(mon => (new DoobieH2TestMapping(transactor, mon) with SqlWorldMapping[IO], mon))

  def simpleRestrictedQuerySql: String =
    "SELECT country.code , country.name FROM country WHERE (( country.code = ?) )"

  def simpleFilteredQuerySql: String =
    "SELECT city.id , city.name FROM city WHERE (city.name ILIKE ?)"

  def filterArg: String = "Linh%"
}
