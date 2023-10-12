// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.sql.test

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlEmbedding2Suite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("paging") {
    val query = """
      query {
        program(programId: "foo") {
          id
          observations {
            matches {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "program" : {
            "id" : "foo",
            "observations" : {
              "matches" : [
                {
                  "id" : "fo1"
                },
                {
                  "id" : "fo2"
                }
              ]
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
