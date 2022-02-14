// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import cats.effect.IO
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import cats.effect.unsafe.implicits.global

import edu.gemini.grackle._
import syntax._
import edu.gemini.grackle.sql.SqlConnectionMapping
import Query._, Value._

trait SqlWorldConnectionMapping[F[_]] extends SqlConnectionMapping[F] {

  trait CountryTable {
    def code:           Column.ColumnRef
    def name:           Column.ColumnRef
    def continent:      Column.ColumnRef
    def region:         Column.ColumnRef
    def surfacearea:    Column.ColumnRef
    def indepyear:      Column.ColumnRef
    def population:     Column.ColumnRef
    def lifeexpectancy: Column.ColumnRef
    def gnp:            Column.ColumnRef
    def gnpold:         Column.ColumnRef
    def localname:      Column.ColumnRef
    def governmentform: Column.ColumnRef
    def headofstate:    Column.ColumnRef
    def capitalId:      Column.ColumnRef
    def code2:          Column.ColumnRef
  }

  def country: CountryTable

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

trait SqlWorldConnectiondSpec extends AnyFunSuite {

  def mapping: QueryExecutor[IO, Json]

  test("connection query with some renames") {

    val query = """
      query {
        countries(first: 5, after: "#<3>") {
          pageInfo {
            hasPreviousPage
            renamedHasNextPage: hasNextPage
            startCursor
            endCursor
          }
          edges {
            node {
              name
              continent
              renamedRegion: region
              population
            }
            cursor
          }
        }
      }
    """

    val expected =
      json"""
      {
        "data" : {
          "countries" : {
            "pageInfo" : {
              "hasPreviousPage" : true,
              "renamedHasNextPage" : true,
              "startCursor" : "#<3>",
              "endCursor" : "#<7>"
            },
            "edges" : [
              {
                "node" : {
                  "name" : "Anguilla",
                  "continent" : "North America",
                  "renamedRegion" : "Caribbean",
                  "population" : 8000
                },
                "cursor" : "#<3>"
              },
              {
                "node" : {
                  "name" : "Albania",
                  "continent" : "Europe",
                  "renamedRegion" : "Southern Europe",
                  "population" : 3401200
                },
                "cursor" : "#<4>"
              },
              {
                "node" : {
                  "name" : "Andorra",
                  "continent" : "Europe",
                  "renamedRegion" : "Southern Europe",
                  "population" : 78000
                },
                "cursor" : "#<5>"
              },
              {
                "node" : {
                  "name" : "Netherlands Antilles",
                  "continent" : "North America",
                  "renamedRegion" : "Caribbean",
                  "population" : 217000
                },
                "cursor" : "#<6>"
              },
              {
                "node" : {
                  "name" : "United Arab Emirates",
                  "continent" : "Asia",
                  "renamedRegion" : "Middle East",
                  "population" : 2441000
                },
                "cursor" : "#<7>"
              }
            ]
          }
        }
      }
      """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    assert(res == expected)
  }

}
