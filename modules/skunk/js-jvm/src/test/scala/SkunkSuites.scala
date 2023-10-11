// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.skunk.test

import cats.effect.IO
import skunk.codec.{all => codec}
import skunk.implicits._

import edu.gemini.grackle.skunk.SkunkMonitor
import edu.gemini.grackle.sql.SqlStatsMonitor

import edu.gemini.grackle.sql.test._
import edu.gemini.grackle.Mapping

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
      def entityType: Codec =
        (codec.int4.imap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class InterfacesSuite2 extends SkunkDatabaseSuite with SqlInterfacesSuite2 {
  lazy val mapping =
    new SkunkTestMapping(pool) with SqlInterfacesMapping2[IO] {
      def entityType: Codec =
        (codec.int4.imap(EntityType.fromInt)(EntityType.toInt), false)
    }
}

final class JsonbSuite extends SkunkDatabaseSuite with SqlJsonbSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlJsonbMapping[IO]
}

final class LikeSuite extends SkunkDatabaseSuite with SqlLikeSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlLikeMapping[IO]
}

final class MixedSuite extends SkunkDatabaseSuite with SqlMixedSuite {
  lazy val mapping = new SkunkTestMapping(pool) with SqlMixedMapping[IO]
}

final class MovieSuite extends SkunkDatabaseSuite with SqlMovieSuite {
  lazy val mapping =
    new SkunkTestMapping(pool) with SqlMovieMapping[IO] {
      def genre: Codec = (codec.int4.imap(Genre.fromInt)(Genre.toInt), false)
      def feature: Codec = (codec.varchar.imap(Feature.fromString)(_.toString), false)
    }
}

final class MutationSuite extends SkunkDatabaseSuite with SqlMutationSuite {
  lazy val mapping =
    new SkunkTestMapping(pool) with SqlMutationMapping[IO] {
      def updatePopulation(id: Int, population: Int): IO[Unit] =
        pool.use { s =>
          s.prepareR(sql"update city set population=${codec.int4} where id=${codec.int4}".command).use { ps =>
            ps.execute(population *: id *: EmptyTuple).void
          }
        }

      def createCity(name: String, countryCode: String, population: Int): IO[Int] =
        pool.use { s =>
          val q = sql"""
              INSERT INTO city (id, name, countrycode, district, population)
              VALUES (nextval('city_id'), ${codec.varchar}, ${codec.bpchar(3)}, 'ignored', ${codec.int4})
              RETURNING id
            """.query(codec.int4)
          s.prepareR(q).use { ps =>
            ps.unique(name *: countryCode *: population *: EmptyTuple)
          }
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
      def itemType: Codec =
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
}

// Needed to avoid an unused import warning in Scala 3.3.0+
object Compat {
  type Dummy = TwiddleCompat
}
