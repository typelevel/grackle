// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import io.circe.Json
import munit.CatsEffectSuite

import edu.gemini.grackle._
import syntax._

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlTreeSuite extends CatsEffectSuite {
  def mapping: QueryExecutor[IO, Json]

  test("root query") {
    val query = """
      query {
        bintree(id: 0) {
          id
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "bintree" : {
            "id" : 0
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("child query (1)") {
    val query = """
      query {
        bintree(id: 0) {
          id
          left {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "bintree" : {
            "id" : 0,
            "left" : {
              "id" : 1
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("child query (2)") {
    val query = """
      query {
        bintree(id: 0) {
          id
          left {
            id
          }
          right {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "bintree" : {
            "id" : 0,
            "left" : {
              "id" : 1
            },
            "right" : {
              "id" : 2
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("child query (3)") {
    val query = """
      query {
        bintree(id: 0) {
          id
          left {
            id
            left {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "bintree" : {
            "id" : 0,
            "left" : {
              "id" : 1,
              "left" : {
                "id" : 3
              }
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("child query (4)") {
    val query = """
      query {
        bintree(id: 0) {
          id
          left {
            id
            left {
              id
            }
            right {
              id
            }
          }
          right {
            id
            left {
              id
            }
            right {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "bintree" : {
            "id" : 0,
            "left" : {
              "id" : 1,
              "left" : {
                "id" : 3
              },
              "right" : {
                "id" : 4
              }
            },
            "right" : {
              "id" : 2,
              "left" : {
                "id" : 5
              },
              "right" : {
                "id" : 6
              }
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
