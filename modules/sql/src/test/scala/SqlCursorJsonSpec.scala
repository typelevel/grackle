// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import io.circe.Json
import cats.effect.IO
import org.scalatest.funsuite.AnyFunSuite
import cats.effect.unsafe.implicits.global
import edu.gemini.grackle._
import GraphQLResponseTests.assertWeaklyEqual
import syntax._

trait SqlCursorJsonSpec extends AnyFunSuite {

  def mapping: QueryExecutor[IO, Json]

  test("cursor field returns json") {
    val query =
      """
      query {
        brands(id: 1) {
          categories {
            name
          }
        }
      }
    """

    val expected =
      json"""
      {
        "data" : {
          "brands" : {
            "categories" : [
              {
                "name" : "Film"
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    println(res)

    assertWeaklyEqual(res, expected)
  }
}