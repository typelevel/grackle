// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

import grackle.test.GraphQLResponseTests.assertWeaklyEqual

trait SqlEmbedding3Spec extends AnyFunSuite {
  def mapping: QueryExecutor[IO, Json]

  test("paging") {
    val query = """
      query {
        observations {
          matches {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "observations" : {
            "matches" : [
              {
                "id" : "fo1"
              },
              {
                "id" : "bo2"
              },
              {
                "id" : "bo1"
              },
              {
                "id" : "fo2"
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
