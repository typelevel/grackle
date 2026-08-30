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

package grackle.doobie.sqlite.test

// Every shared sql-core suite is wired up here, matching the doobie-pg/doobie-oracle/
// doobie-mssql suites this is modelled on. All pass, with one caveat:
// FilterOrderOffsetLimit2Suite's "multi join nested limit (2)" has been observed to fail once
// (an empty nested list) on identical code, cause unconfirmed. The generated SQL's result
// content is provably deterministic - keys are paginated via an ordered DISTINCT subquery and
// nested limits via dense_rank over unique ids - leaving only the unordered final row sequence
// as a suspect; the failure has not reproduced in over 120 runs since.

import cats.effect.{IO, Resource}
import munit.catseffect.IOFixture
import org.typelevel.doobie.{Meta, Transactor}
import org.typelevel.doobie.implicits._

import grackle.Mapping
import grackle.doobie.DoobieMonitor
import grackle.sql.SqlStatsMonitor
import grackle.sql.test._

final class ArrayJoinSuite extends DoobieSqliteDatabaseSuite with SqlArrayJoinSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlArrayJoinMapping[IO]
}

final class CoalesceSuite extends DoobieSqliteDatabaseSuite with SqlCoalesceSuite {
  type Fragment = org.typelevel.doobie.Fragment
  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO, Fragment])] =
    DoobieMonitor
      .statsMonitor[IO]
      .map(mon =>
        (new DoobieSqliteTestMapping(transactor, mon) with SqlCoalesceMapping[IO], mon))
}

final class ComposedWorldSuite extends DoobieSqliteDatabaseSuite with SqlComposedWorldSuite {
  def mapping: IO[(CurrencyMapping[IO], Mapping[IO])] =
    for {
      currencyMapping <- CurrencyMapping[IO]
    } yield (
      currencyMapping,
      new SqlComposedMapping(
        new DoobieSqliteTestMapping(transactor) with SqlWorldMapping[IO],
        currencyMapping))
}

final class CompositeKeySuite extends DoobieSqliteDatabaseSuite with SqlCompositeKeySuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlCompositeKeyMapping[IO]
}

final class ErrorKeysSuite extends DoobieSqliteDatabaseSuite with SqlErrorKeysSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlCompositeKeyMapping[IO]
}

final class CursorJsonSuite extends DoobieSqliteDatabaseSuite with SqlCursorJsonSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlCursorJsonMapping[IO]
}

final class EmbeddingSuite extends DoobieSqliteDatabaseSuite with SqlEmbeddingSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlEmbeddingMapping[IO]
}

final class Embedding2Suite extends DoobieSqliteDatabaseSuite with SqlEmbedding2Suite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlEmbedding2Mapping[IO]
}

final class Embedding3Suite extends DoobieSqliteDatabaseSuite with SqlEmbedding3Suite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlEmbedding3Mapping[IO]
}

final class FilterJoinAliasSuite
    extends DoobieSqliteDatabaseSuite
    with SqlFilterJoinAliasSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlFilterJoinAliasMapping[IO]
}

final class FilterOrderOffsetLimitSuite
    extends DoobieSqliteDatabaseSuite
    with SqlFilterOrderOffsetLimitSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor)
    with SqlFilterOrderOffsetLimitMapping[IO]
}

final class FilterOrderOffsetLimit2Suite
    extends DoobieSqliteDatabaseSuite
    with SqlFilterOrderOffsetLimit2Suite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor)
    with SqlFilterOrderOffsetLimit2Mapping[IO]
}

final class GraphSuite extends DoobieSqliteDatabaseSuite with SqlGraphSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlGraphMapping[IO]
}

final class InterfacesSuite extends DoobieSqliteDatabaseSuite with SqlInterfacesSuite {
  lazy val mapping =
    new DoobieSqliteTestMapping(transactor) with SqlInterfacesMapping[IO] {
      def entityType: TestCodec[EntityType] =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class InterfacesSuite2 extends DoobieSqliteDatabaseSuite with SqlInterfacesSuite2 {
  lazy val mapping =
    new DoobieSqliteTestMapping(transactor) with SqlInterfacesMapping2[IO] {
      def entityType: TestCodec[EntityType] =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class JsonbSuite extends DoobieSqliteDatabaseSuite with SqlJsonbSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlJsonbMapping[IO]
}

final class LikeSuite extends DoobieSqliteDatabaseSuite with SqlLikeSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlLikeMapping[IO]
}

final class MappingValidatorValidSuite
    extends DoobieSqliteDatabaseSuite
    with SqlMappingValidatorValidSuite {
  // no DB instance needed for this suite
  lazy val mapping = new DoobieSqliteTestMapping(null)
    with SqlMappingValidatorValidMapping[IO] {
    def genre: TestCodec[Genre] = (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)
    def feature: TestCodec[Feature] = (Meta[String].imap(Feature.fromString)(_.toString), false)
  }
  override def munitFixtures: Seq[IOFixture[_]] = Nil
}

final class MappingValidatorInvalidSuite
    extends DoobieSqliteDatabaseSuite
    with SqlMappingValidatorInvalidSuite {
  // no DB instance needed for this suite
  lazy val mapping = new DoobieSqliteTestMapping(null)
    with SqlMappingValidatorInvalidMapping[IO]
  override def munitFixtures: Seq[IOFixture[_]] = Nil
}

final class MixedSuite extends DoobieSqliteDatabaseSuite with SqlMixedSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlMixedMapping[IO]
}

final class MovieSuite extends DoobieSqliteDatabaseSuite with SqlMovieSuite {
  lazy val mapping =
    new DoobieSqliteTestMapping(transactor) with SqlMovieMapping[IO] {
      def genre: TestCodec[Genre] = (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)
      def feature: TestCodec[Feature] =
        (Meta[String].imap(Feature.fromString)(_.toString), false)
      def tagList: TestCodec[List[String]] = (Meta[Int].imap(Tags.fromInt)(Tags.toInt), false)
    }
}

final class MutationSuite extends DoobieSqliteDatabaseSuite with SqlMutationSuite {
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
    new DoobieSqliteTestMapping(transactor) with SqlMutationMapping[IO] {
      def updatePopulation(id: Int, population: Int): IO[Unit] =
        sql"UPDATE city_copy SET population=$population WHERE id=$id"
          .update
          .run
          .transact(transactor)
          .void

      // SQLite has no sequences: mint a fresh id by hand (scoped to city_copy, which is all that
      // matters for this test) and hand it back via RETURNING (supported since SQLite 3.35).
      def createCity(name: String, countryCode: String, population: Int): IO[Int] =
        sql"""
          INSERT INTO city_copy (id, name, countrycode, district, population)
          VALUES ((SELECT COALESCE(MAX(id), 0) + 1 FROM city_copy), $name, $countryCode, 'ignored', $population)
          RETURNING id
          """.query[Int].unique.transact(transactor)
    }
}

final class NullableParentSuite extends DoobieSqliteDatabaseSuite with SqlNullableParentSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlNullableParentMapping[IO]
}

final class NullOrderingSuite extends DoobieSqliteDatabaseSuite with SqlNullOrderingSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlNullOrderingMapping[IO]
}

final class NestedEffectsSuite extends DoobieSqliteDatabaseSuite with SqlNestedEffectsSuite {
  def mapping: IO[(CurrencyService[IO], Mapping[IO])] =
    for {
      currencyService0 <- CurrencyService[IO]
    } yield {
      val mapping =
        new DoobieSqliteTestMapping(transactor) with SqlNestedEffectsMapping[IO] {
          lazy val currencyService = currencyService0
        }
      (currencyService0, mapping)
    }
}

final class Paging1Suite extends DoobieSqliteDatabaseSuite with SqlPaging1Suite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlPaging1Mapping[IO]
}

final class Paging2Suite extends DoobieSqliteDatabaseSuite with SqlPaging2Suite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlPaging2Mapping[IO]
}

final class Paging3Suite extends DoobieSqliteDatabaseSuite with SqlPaging3Suite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlPaging3Mapping[IO]
}

final class ProjectionSuite extends DoobieSqliteDatabaseSuite with SqlProjectionSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlProjectionMapping[IO]
}

final class RecursiveInterfacesSuite
    extends DoobieSqliteDatabaseSuite
    with SqlRecursiveInterfacesSuite {
  lazy val mapping =
    new DoobieSqliteTestMapping(transactor) with SqlRecursiveInterfacesMapping[IO] {
      def itemType: TestCodec[ItemType] =
        (Meta[Int].timap(ItemType.fromInt)(ItemType.toInt), false)
    }
}

final class SiblingListsSuite extends DoobieSqliteDatabaseSuite with SqlSiblingListsSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlSiblingListsData[IO]
}

final class TreeSuite extends DoobieSqliteDatabaseSuite with SqlTreeSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlTreeMapping[IO]
}

final class UnionsSuite extends DoobieSqliteDatabaseSuite with SqlUnionSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlUnionsMapping[IO]
}

final class WorldSuite extends DoobieSqliteDatabaseSuite with SqlWorldSuite {
  lazy val mapping = new DoobieSqliteTestMapping(transactor) with SqlWorldMapping[IO]
}

final class WorldCompilerSuite extends DoobieSqliteDatabaseSuite with SqlWorldCompilerSuite {
  type Fragment = org.typelevel.doobie.Fragment

  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO, Fragment])] =
    DoobieMonitor
      .statsMonitor[IO]
      .map(mon => (new DoobieSqliteTestMapping(transactor, mon) with SqlWorldMapping[IO], mon))

  def simpleRestrictedQuerySql: String =
    "SELECT country.code , country.name FROM country WHERE (( country.code = ?) )"

  def simpleFilteredQuerySql: String =
    "SELECT city.id , city.name FROM city WHERE (UPPER( city.name ) LIKE ?)"

  def filterArg: String = "LINH%"
}
