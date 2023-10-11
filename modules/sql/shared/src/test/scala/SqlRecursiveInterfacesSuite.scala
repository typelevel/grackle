// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import edu.gemini.grackle._

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlRecursiveInterfacesSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("specialized query on both sides") {
    val query = """
      query {
        items {
          id
          ... on ItemA {
            nextItem {
              id
            }
          }
          ... on ItemB {
            nextItem {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "items" : [
            {
              "id" : "1",
              "nextItem" : {
                "id" : "2"
              }
            },
            {
              "id" : "2",
              "nextItem" : {
                "id" : "1"
              }
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
