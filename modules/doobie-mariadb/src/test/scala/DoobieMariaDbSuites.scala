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

package grackle.doobie.mariadb.test

import cats.effect.{IO, Resource}
import cats.syntax.all._
import munit.catseffect.IOFixture
import org.typelevel.doobie.{Meta, Transactor}
import org.typelevel.doobie.implicits._

import grackle.Mapping
import grackle.doobie.DoobieMonitor
import grackle.sql.SqlStatsMonitor
import grackle.sql.test._

final class ArrayJoinSuite extends DoobieMariaDbDatabaseSuite with SqlArrayJoinSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlArrayJoinMapping[IO]
}

final class CoalesceSuite extends DoobieMariaDbDatabaseSuite with SqlCoalesceSuite {
  type Fragment = org.typelevel.doobie.Fragment
  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO, Fragment])] =
    DoobieMonitor
      .statsMonitor[IO]
      .map(mon =>
        (new DoobieMariaDbTestMapping(transactor, mon) with SqlCoalesceMapping[IO], mon))
}

final class ComposedWorldSuite extends DoobieMariaDbDatabaseSuite with SqlComposedWorldSuite {
  def mapping: IO[(CurrencyMapping[IO], Mapping[IO])] =
    for {
      currencyMapping <- CurrencyMapping[IO]
    } yield (
      currencyMapping,
      new SqlComposedMapping(
        new DoobieMariaDbTestMapping(transactor) with SqlWorldMapping[IO],
        currencyMapping))
}

final class CompositeKeySuite extends DoobieMariaDbDatabaseSuite with SqlCompositeKeySuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlCompositeKeyMapping[IO]
}

final class CursorJsonSuite extends DoobieMariaDbDatabaseSuite with SqlCursorJsonSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlCursorJsonMapping[IO]
}

final class EmbeddingSuite extends DoobieMariaDbDatabaseSuite with SqlEmbeddingSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlEmbeddingMapping[IO]
}

final class Embedding2Suite extends DoobieMariaDbDatabaseSuite with SqlEmbedding2Suite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlEmbedding2Mapping[IO]
}

final class Embedding3Suite extends DoobieMariaDbDatabaseSuite with SqlEmbedding3Suite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlEmbedding3Mapping[IO]
}

final class FilterJoinAliasSuite
    extends DoobieMariaDbDatabaseSuite
    with SqlFilterJoinAliasSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlFilterJoinAliasMapping[IO]
}

final class FilterOrderOffsetLimitSuite
    extends DoobieMariaDbDatabaseSuite
    with SqlFilterOrderOffsetLimitSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor)
    with SqlFilterOrderOffsetLimitMapping[IO]
}

final class FilterOrderOffsetLimit2Suite
    extends DoobieMariaDbDatabaseSuite
    with SqlFilterOrderOffsetLimit2Suite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor)
    with SqlFilterOrderOffsetLimit2Mapping[IO]
}

final class GraphSuite extends DoobieMariaDbDatabaseSuite with SqlGraphSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlGraphMapping[IO]
}

final class InterfacesSuite extends DoobieMariaDbDatabaseSuite with SqlInterfacesSuite {
  lazy val mapping =
    new DoobieMariaDbTestMapping(transactor) with SqlInterfacesMapping[IO] {
      def entityType: TestCodec[EntityType] =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class InterfacesSuite2 extends DoobieMariaDbDatabaseSuite with SqlInterfacesSuite2 {
  lazy val mapping =
    new DoobieMariaDbTestMapping(transactor) with SqlInterfacesMapping2[IO] {
      def entityType: TestCodec[EntityType] =
        (Meta[Int].timap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class JsonbSuite extends DoobieMariaDbDatabaseSuite with SqlJsonbSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlJsonbMapping[IO]
}

final class LikeSuite extends DoobieMariaDbDatabaseSuite with SqlLikeSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlLikeMapping[IO]
}

final class MappingValidatorValidSuite
    extends DoobieMariaDbDatabaseSuite
    with SqlMappingValidatorValidSuite {
  // no DB instance needed for this suite
  lazy val mapping = new DoobieMariaDbTestMapping(null)
    with SqlMappingValidatorValidMapping[IO] {
    def genre: TestCodec[Genre] = (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)
    def feature: TestCodec[Feature] = (Meta[String].imap(Feature.fromString)(_.toString), false)
  }
  override def munitFixtures: Seq[IOFixture[_]] = Nil
}

final class MappingValidatorInvalidSuite
    extends DoobieMariaDbDatabaseSuite
    with SqlMappingValidatorInvalidSuite {
  // no DB instance needed for this suite
  lazy val mapping = new DoobieMariaDbTestMapping(null)
    with SqlMappingValidatorInvalidMapping[IO]
  override def munitFixtures: Seq[IOFixture[_]] = Nil
}

final class MixedSuite extends DoobieMariaDbDatabaseSuite with SqlMixedSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlMixedMapping[IO]
}

final class MovieSuite extends DoobieMariaDbDatabaseSuite with SqlMovieSuite {
  lazy val mapping =
    new DoobieMariaDbTestMapping(transactor) with SqlMovieMapping[IO] {
      def genre: TestCodec[Genre] = (Meta[Int].imap(Genre.fromInt)(Genre.toInt), false)
      def feature: TestCodec[Feature] =
        (Meta[String].imap(Feature.fromString)(_.toString), false)
      def tagList: TestCodec[List[String]] = (Meta[Int].imap(Tags.fromInt)(Tags.toInt), false)
    }
}

final class MutationSuite extends DoobieMariaDbDatabaseSuite with SqlMutationSuite {
  // A resource that copies and drops the table used in the tests. MariaDB has neither
  // CREATE TABLE ... AS SELECT with constraints preserved, nor sequences: city_copy is built
  // explicitly with an AUTO_INCREMENT primary key (which self-adjusts past the copied max id),
  // and createCity below mints ids via JDBC generated keys instead of RETURNING. Column order
  // must match city's own (id, name, countrycode, district, population) since the copy below is
  // a positional `SELECT *`.
  // Nb. the statements run in order but not atomically: MariaDB commits implicitly on DDL, so the
  // CREATE is already committed by the time the seed INSERT runs - which also means an
  // interrupted run leaves city_copy behind in the shared container, hence the leading DROP.
  def withDuplicatedTables(transactor: Transactor[IO]): Resource[IO, Transactor[IO]] = {
    val alloc =
      (sql"DROP TABLE IF EXISTS city_copy".update.run *>
        sql"""
        CREATE TABLE city_copy (
          id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
          name VARCHAR(100) NOT NULL,
          countrycode VARCHAR(3) NOT NULL,
          district VARCHAR(100) NOT NULL,
          population INT NOT NULL
        )
      """.update.run *>
        sql"INSERT INTO city_copy SELECT * FROM city".update.run)
        .transact(transactor)
        .as(transactor)
    val free = sql"DROP TABLE city_copy".update.run.transact(transactor).void
    Resource.make(alloc)(_ => free)
  }

  override def transactorResource: Resource[IO, Transactor[IO]] =
    super.transactorResource.flatMap(withDuplicatedTables)

  lazy val mapping =
    new DoobieMariaDbTestMapping(transactor) with SqlMutationMapping[IO] {
      def updatePopulation(id: Int, population: Int): IO[Unit] =
        sql"UPDATE city_copy SET population=$population WHERE id=$id"
          .update
          .run
          .transact(transactor)
          .void

      def createCity(name: String, countryCode: String, population: Int): IO[Int] =
        sql"""
          INSERT INTO city_copy (countrycode, name, district, population)
          VALUES ($countryCode, $name, 'ignored', $population)
        """.update.withUniqueGeneratedKeys[Int]("id").transact(transactor)
    }
}

final class NestedEffectsSuite extends DoobieMariaDbDatabaseSuite with SqlNestedEffectsSuite {
  def mapping: IO[(CurrencyService[IO], Mapping[IO])] =
    for {
      currencyService0 <- CurrencyService[IO]
    } yield {
      val mapping =
        new DoobieMariaDbTestMapping(transactor) with SqlNestedEffectsMapping[IO] {
          lazy val currencyService = currencyService0
        }
      (currencyService0, mapping)
    }
}

final class Paging1Suite extends DoobieMariaDbDatabaseSuite with SqlPaging1Suite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlPaging1Mapping[IO]
}

final class Paging2Suite extends DoobieMariaDbDatabaseSuite with SqlPaging2Suite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlPaging2Mapping[IO]
}

final class Paging3Suite extends DoobieMariaDbDatabaseSuite with SqlPaging3Suite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlPaging3Mapping[IO]
}

final class ProjectionSuite extends DoobieMariaDbDatabaseSuite with SqlProjectionSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlProjectionMapping[IO]
}

final class RecursiveInterfacesSuite
    extends DoobieMariaDbDatabaseSuite
    with SqlRecursiveInterfacesSuite {
  lazy val mapping =
    new DoobieMariaDbTestMapping(transactor) with SqlRecursiveInterfacesMapping[IO] {
      def itemType: TestCodec[ItemType] =
        (Meta[Int].timap(ItemType.fromInt)(ItemType.toInt), false)
    }
}

final class SiblingListsSuite extends DoobieMariaDbDatabaseSuite with SqlSiblingListsSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlSiblingListsData[IO]
}

final class TreeSuite extends DoobieMariaDbDatabaseSuite with SqlTreeSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlTreeMapping[IO]
}

final class UnionsSuite extends DoobieMariaDbDatabaseSuite with SqlUnionSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlUnionsMapping[IO]
}

final class WorldSuite extends DoobieMariaDbDatabaseSuite with SqlWorldSuite {
  lazy val mapping = new DoobieMariaDbTestMapping(transactor) with SqlWorldMapping[IO]
}

final class WorldCompilerSuite extends DoobieMariaDbDatabaseSuite with SqlWorldCompilerSuite {
  type Fragment = org.typelevel.doobie.Fragment

  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO, Fragment])] =
    DoobieMonitor
      .statsMonitor[IO]
      .map(mon => (new DoobieMariaDbTestMapping(transactor, mon) with SqlWorldMapping[IO], mon))

  def simpleRestrictedQuerySql: String =
    "SELECT country.code , country.name FROM country WHERE (( country.code = ?) )"

  def simpleFilteredQuerySql: String =
    "SELECT city.id , city.name FROM city WHERE (UPPER( city.name ) LIKE ?)"

  def filterArg: String = "LINH%"
}
