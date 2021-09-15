// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import cats.effect.IO
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import cats.effect.unsafe.implicits.global

import edu.gemini.grackle._
import syntax._

import GraphQLResponseTests.assertWeaklyEqual

trait SqlGraphSpec extends AnyFunSuite {
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

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
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

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
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

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }
}
