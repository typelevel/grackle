// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import cats.effect.IO
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import cats.effect.unsafe.implicits.global

import edu.gemini.grackle._
import syntax._

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
