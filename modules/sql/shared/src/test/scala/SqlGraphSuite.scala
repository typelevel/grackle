// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import io.circe.Json
import munit.CatsEffectSuite

import edu.gemini.grackle._
import syntax._

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlGraphSuite extends CatsEffectSuite {
  def mapping: QueryExecutor[IO, Json]

  test("root query") {
    val query = """
      query {
        node(id: 1) {
          id
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "node" : {
            "id" : 1
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("neighbour query (1)") {
    val query = """
      query {
        node(id: 1) {
          id
          neighbours {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "node" : {
            "id" : 1,
            "neighbours" : [
              {
                "id" : 2
              },
              {
                "id" : 3
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("neighbour query (2)") {
    val query = """
      query {
        node(id: 1) {
          id
          neighbours {
            id
            neighbours {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "node" : {
            "id" : 1,
            "neighbours" : [
              {
                "id" : 2,
                "neighbours" : [
                  {
                    "id" : 4
                  }
                ]
              },
              {
                "id" : 3,
                "neighbours" : [
                  {
                    "id" : 4
                  }
                ]
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
