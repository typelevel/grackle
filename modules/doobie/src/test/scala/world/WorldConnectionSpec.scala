// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.Sync
import cats.implicits._

import doobie.util.meta.Meta
import doobie.util.transactor.Transactor

import edu.gemini.grackle._
import doobie.{DoobieMapping, DoobieMappingCompanion, DoobieMonitor}
import syntax._
import Query._, Value._
import edu.gemini.grackle.sql.SqlConnectionMapping
import utils.DatabaseSuite
import grackle.test.SqlWorldConnectiondSpec

trait WorldConnectionMapping[F[_]] extends DoobieMapping[F] with SqlConnectionMapping[F] {

  object country extends TableDef("country") {
    val code           = col("code", Meta[String])
    val name           = col("name", Meta[String])
    val continent      = col("continent", Meta[String])
    val region         = col("region", Meta[String])
    val surfacearea    = col("surfacearea", Meta[String])
    val indepyear      = col("indepyear", Meta[Int], true)
    val population     = col("population", Meta[Int])
    val lifeexpectancy = col("lifeexpectancy", Meta[String], true)
    val gnp            = col("gnp", Meta[String], true)
    val gnpold         = col("gnpold", Meta[String], true)
    val localname      = col("localname", Meta[String])
    val governmentform = col("governmentform", Meta[String])
    val headofstate    = col("headofstate", Meta[String], true)
    val capitalId      = col("capitalId", Meta[String], true)
    val code2          = col("code2", Meta[String])
  }

  val schema =
    schema"""

      type Query {
        countries(first: Int!, after: Cursor): CountryConnection!
      }

      type Country {
        name: String!
        continent: String!
        region: String!
        surfacearea: Float!
        indepyear: Int
        population: Int!
        lifeexpectancy: Float
        gnp: String
        gnpold: String
        localname: String!
        governmentform: String!
        headofstate: String
        capitalId: Int
        code2: String!
      }

      scalar Cursor

      type PageInfo {
        hasPreviousPage: Boolean!
        hasNextPage: Boolean!
        startCursor: Cursor!
        endCursor: Cursor!
      }

      type CountryConnection {
        edges: [CountryEdge!]!
        pageInfo: PageInfo!
      }

      type CountryEdge {
        node: Country!
        cursor: Cursor!
      }

    """

  val QueryType    = schema.ref("Query")
  val CountryType  = schema.ref("Country")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlConnection("countries"),
        )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings = List(
          SqlField("code",           country.code, key = true, hidden = true),
          SqlField("name",           country.name),
          SqlField("continent",      country.continent),
          SqlField("region",         country.region),
          SqlField("surfacearea",    country.surfacearea),
          SqlField("indepyear",      country.indepyear),
          SqlField("population",     country.population),
          SqlField("lifeexpectancy", country.lifeexpectancy),
          SqlField("gnp",            country.gnp),
          SqlField("gnpold",         country.gnpold),
          SqlField("localname",      country.localname),
          SqlField("governmentform", country.governmentform),
          SqlField("headofstate",    country.headofstate),
          SqlField("capitalId",      country.capitalId),
          SqlField("code2",          country.code2),
        ),
      ),
    )

  object First {
    def unapply(b: Binding): Option[Int] =
      b match {
        case Binding("first", IntValue(n)) => Some(n)
        case _                             => None
      }
  }

  object After {
    def unapply(b: Binding): Option[Option[String]] =
      b match {
        case Binding("after", StringValue(s))          => Some(Some(s))
        case Binding("after", NullValue | AbsentValue) => Some(None)
        case _                                         => None
      }
  }

  override val selectElaborator: QueryCompiler.SelectElaborator =
    new QueryCompiler.SelectElaborator(
      Map {
        QueryType -> {
          case q @ Query.Select("countries", List(First(first), After(after)), _) =>
            SqlConnectionMapping.updateNodeQuery(q, first, after) {
              case Select("node", Nil, child) =>
                val ord = Query.OrderBy(OrderSelections(List(OrderSelection(Path.UniquePath[String](List("code"))))), child)
                Result(Select("node", Nil, ord))
              case o => Result.failure(s"SqlConnectionMapping.updateNodeQuery didn't match $o")
            }
        }
      }
    )

}

object WorldConnectionMapping extends DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with WorldConnectionMapping[F]
}

final class WorldConnectionSpec extends DatabaseSuite with SqlWorldConnectiondSpec {
  lazy val mapping = WorldConnectionMapping.fromTransactor(xa)
}
