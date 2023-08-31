// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import edu.gemini.grackle._
import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlCursorJsonSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
