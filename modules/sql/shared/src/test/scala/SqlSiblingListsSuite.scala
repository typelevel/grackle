// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import edu.gemini.grackle._

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlSiblingListsSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("base query") {
    val query = """
      query {
        a(id: "id_a_1") {
          bs {
            cs { nameC }
            ds { nameD }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "a" : {
            "bs" : [
              {
                "cs" : [
                  {
                    "nameC" : "name_c"
                  }
                ],
                "ds" : [
                  {
                    "nameD" : "name_d"
                  }
                ]
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)
    //println(res)

    assertWeaklyEqualIO(res, expected)
  }
}
