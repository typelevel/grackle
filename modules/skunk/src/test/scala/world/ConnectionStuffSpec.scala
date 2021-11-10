// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.{Resource, Sync}
import skunk.Session

import edu.gemini.grackle._, skunk._, syntax._
import utils.DatabaseSuite
import _root_.skunk.codec.all._
import cats.effect.unsafe.implicits.global

trait ConnectionStuffMapping[F[_]] extends SkunkMapping[F] with SqlConnectionMapping[F] {

  object country extends TableDef("country") {
    val code           = col("code", bpchar(3))
    val name           = col("name", text)
    val continent      = col("continent", text)
    val region         = col("region", text)
    val surfacearea    = col("surfacearea", varchar)
    val indepyear      = col("indepyear", int2.imap(_.toInt)(_.toShort).opt)
    val population     = col("population", int4)
    val lifeexpectancy = col("lifeexpectancy", varchar.opt)
    val gnp            = col("gnp", varchar.opt)
    val gnpold         = col("gnpold", varchar.opt)
    val localname      = col("localname", varchar)
    val governmentform = col("governmentform", varchar)
    val headofstate    = col("headofstate", varchar.opt)
    val capitalId      = col("capitalId", varchar.opt)
    val code2          = col("code2", varchar)
  }

  val schema =
    schema"""

      type Query {
        countries(first: Int, after: Cursor): CountryConnection!
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

}

object ConnectionStuffMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F,Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with ConnectionStuffMapping[F]
}

final class ConnectionStuffSuite extends DatabaseSuite {
  lazy val mapping = ConnectionStuffMapping.mkMapping(pool)

  test("connection query with some renames") {

    val query = """
      query {
        countries {
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
                  "name" : "Netherlands Antilles",
                  "continent" : "North America",
                  "renamedRegion" : "Caribbean",
                  "population" : 217000
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
                  "name" : "Anguilla",
                  "continent" : "North America",
                  "renamedRegion" : "Caribbean",
                  "population" : 8000
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
