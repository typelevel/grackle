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

package grackle.skunk.test

import cats.effect.{IO, Resource}
import munit.catseffect.IOFixture
import skunk.Session
import skunk.codec.{all => codec}
import skunk.implicits._

import grackle.skunk.SkunkMonitor
import grackle.sql.SqlStatsMonitor

import grackle.sql.test._
import grackle.Mapping

import org.typelevel.twiddles._

final class ArrayJoinSuite extends SkunkDatabaseSuite with SqlArrayJoinSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlArrayJoinMapping[IO]
}

final class CoalesceSuite extends SkunkDatabaseSuite with SqlCoalesceSuite {
  type Fragment = skunk.AppliedFragment
  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO,Fragment])] =
    SkunkMonitor.statsMonitor[IO].map(mon => (new SkunkTestMapping(pool, mon) with SqlCoalesceMapping[IO], mon))
}

final class ComposedWorldSuite extends SkunkDatabaseSuite with SqlComposedWorldSuite {
  def mapping: IO[(CurrencyMapping[IO], Mapping[IO])] =
    for {
      currencyMapping <- CurrencyMapping[IO]
    } yield (currencyMapping, new SqlComposedMapping(new SkunkTestMapping(pool) with SqlWorldMapping[IO], currencyMapping))
}

final class CompositeKeySuite extends SkunkDatabaseSuite with SqlCompositeKeySuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlCompositeKeyMapping[IO]
}

final class CursorJsonSuite extends SkunkDatabaseSuite with SqlCursorJsonSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlCursorJsonMapping[IO]
}

final class EmbeddingSuite extends SkunkDatabaseSuite with SqlEmbeddingSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlEmbeddingMapping[IO]
}

final class Embedding2Suite extends SkunkDatabaseSuite with SqlEmbedding2Suite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlEmbedding2Mapping[IO]
}

final class Embedding3Suite extends SkunkDatabaseSuite with SqlEmbedding3Suite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlEmbedding3Mapping[IO]
}

final class FilterJoinAliasSuite extends SkunkDatabaseSuite with SqlFilterJoinAliasSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlFilterJoinAliasMapping[IO]
}

final class FilterOrderOffsetLimitSuite extends SkunkDatabaseSuite with SqlFilterOrderOffsetLimitSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlFilterOrderOffsetLimitMapping[IO]
}

final class FilterOrderOffsetLimit2Suite extends SkunkDatabaseSuite with SqlFilterOrderOffsetLimit2Suite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlFilterOrderOffsetLimit2Mapping[IO]
}

final class GraphSuite extends SkunkDatabaseSuite with SqlGraphSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlGraphMapping[IO]
}

final class InterfacesSuite extends SkunkDatabaseSuite with SqlInterfacesSuite {
  lazy val mapping =
    new SkunkTestMapping(pool) with SqlInterfacesMapping[IO] {
      def entityType: TestCodec[EntityType] =
        (codec.int4.imap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class InterfacesSuite2 extends SkunkDatabaseSuite with SqlInterfacesSuite2 {
  lazy val mapping =
    new SkunkTestMapping(pool) with SqlInterfacesMapping2[IO] {
      def entityType: TestCodec[EntityType] =
        (codec.int4.imap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class JsonbSuite extends SkunkDatabaseSuite with SqlJsonbSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlJsonbMapping[IO]
}

final class LikeSuite extends SkunkDatabaseSuite with SqlLikeSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlLikeMapping[IO]
}

final class MappingValidatorValidSuite extends SkunkDatabaseSuite with SqlMappingValidatorValidSuite {
  // no DB instance needed for this suite
  lazy val mapping =
    new SkunkTestMapping(null) with SqlMappingValidatorValidMapping[IO] {
      def genre: TestCodec[Genre] = (codec.int4.imap(Genre.fromInt)(Genre.toInt), false)
      def feature: TestCodec[Feature] = (codec.varchar.imap(Feature.fromString)(_.toString), false)
    }
  override def munitFixtures: Seq[IOFixture[_]] = Nil
}

final class MappingValidatorInvalidSuite extends SkunkDatabaseSuite with SqlMappingValidatorInvalidSuite {
  // no DB instance needed for this suite
  lazy val mapping = new SkunkTestMapping(null) with SqlMappingValidatorInvalidMapping[IO]
  override def munitFixtures: Seq[IOFixture[_]] = Nil
}

final class MixedSuite extends SkunkDatabaseSuite with SqlMixedSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlMixedMapping[IO]
}

final class MovieSuite extends SkunkDatabaseSuite with SqlMovieSuite {
  lazy val mapping =
    new SkunkTestMapping(pool) with SqlMovieMapping[IO] {
      def genre: TestCodec[Genre] = (codec.int4.imap(Genre.fromInt)(Genre.toInt), false)
      def feature: TestCodec[Feature] = (codec.varchar.imap(Feature.fromString)(_.toString), false)
      def tagList: TestCodec[List[String]] = (codec.int4.imap(Tags.fromInt)(Tags.toInt), false)
    }
}

final class MutationSuite extends SkunkDatabaseSuite with SqlMutationSuite {
  // A resource that copies and drops the table used in the tests.
  def withDuplicatedTables(p: Resource[IO, Session[IO]]): Resource[IO, Resource[IO, Session[IO]]] = {
    val alloc = p.use(_.execute(sql"CREATE TABLE city_copy AS SELECT * FROM city".command)).as(p)
    val free  = p.use(_.execute(sql"DROP TABLE city_copy".command)).void
    Resource.make(alloc)(_ => free)
  }

  override def poolResource: Resource[IO, Resource[IO, Session[IO]]] =
    super.poolResource.flatMap(withDuplicatedTables)

  lazy val mapping =
    new SkunkTestMapping(pool) with SqlMutationMapping[IO] {
      def updatePopulation(id: Int, population: Int): IO[Unit] =
        pool.use(_.prepareR(sql"UPDATE city_copy SET population=${codec.int4} WHERE id=${codec.int4}".command).use { ps =>
          ps.execute(population *: id *: EmptyTuple).void
        })

      def createCity(name: String, countryCode: String, population: Int): IO[Int] = {
        val q = sql"""
            INSERT INTO city_copy (id, name, countrycode, district, population)
            VALUES (nextval('city_id'), ${codec.varchar}, ${codec.bpchar(3)}, 'ignored', ${codec.int4})
            RETURNING id
          """.query(codec.int4)
        pool.use(_.prepareR(q).use { ps =>
          ps.unique(name *: countryCode *: population *: EmptyTuple)
        })
      }
    }
}

final class NestedEffectsSuite extends SkunkDatabaseSuite with SqlNestedEffectsSuite {
  def mapping: IO[(CurrencyService[IO], Mapping[IO])] =
    for {
      currencyService0 <- CurrencyService[IO]
    } yield {
      val mapping =
        new SkunkTestMapping(pool) with SqlNestedEffectsMapping[IO] {
          lazy val currencyService = currencyService0
        }
      (currencyService0, mapping)
    }
}

final class Paging1Suite extends SkunkDatabaseSuite with SqlPaging1Suite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlPaging1Mapping[IO]
}

final class Paging2Suite extends SkunkDatabaseSuite with SqlPaging2Suite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlPaging2Mapping[IO]
}

final class Paging3Suite extends SkunkDatabaseSuite with SqlPaging3Suite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlPaging3Mapping[IO]
}

final class ProjectionSuite extends SkunkDatabaseSuite with SqlProjectionSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlProjectionMapping[IO]
}

final class RecursiveInterfacesSuite extends SkunkDatabaseSuite with SqlRecursiveInterfacesSuite {
  lazy val mapping =
    new SkunkTestMapping(pool) with SqlRecursiveInterfacesMapping[IO] {
      def itemType: TestCodec[ItemType] =
        (codec.int4.imap(ItemType.fromInt)(ItemType.toInt), false)
    }
}

final class SiblingListsSuite extends SkunkDatabaseSuite with SqlSiblingListsSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlSiblingListsData[IO]
}

final class TreeSuite extends SkunkDatabaseSuite with SqlTreeSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlTreeMapping[IO]
}

final class UnionsSuite extends SkunkDatabaseSuite with SqlUnionSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlUnionsMapping[IO]
}

final class WorldSuite extends SkunkDatabaseSuite with SqlWorldSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlWorldMapping[IO]
}

final class WorldCompilerSuite extends SkunkDatabaseSuite with SqlWorldCompilerSuite {
  type Fragment = skunk.AppliedFragment

  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO,Fragment])] =
    SkunkMonitor.statsMonitor[IO].map(mon => (new SkunkTestMapping(pool, mon) with SqlWorldMapping[IO], mon))

  def simpleRestrictedQuerySql: String =
    "SELECT country.code, country.name FROM country WHERE ((country.code = $1))"

  def simpleFilteredQuerySql: String =
    "SELECT city.id, city.name FROM city WHERE (city.name ILIKE $1)"

  def filterArg: String = "Linh%"
}

// Needed to avoid an unused import warning in Scala 3.3.0+
object Compat {
  type Dummy = TwiddleCompat
}
