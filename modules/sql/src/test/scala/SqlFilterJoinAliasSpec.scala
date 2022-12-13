// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite

import edu.gemini.grackle._
import syntax._

import grackle.test.GraphQLResponseTests.assertWeaklyEqual

trait SqlFilterJoinAliasSpec extends AnyFunSuite {
  def mapping: Mapping[IO]

  test("base query") {
    val query = """
      query {
        episode(id: "a") {
          images(filter: { name: "abc" }) {
            inner { name }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "episode" : {
            "images" : [
              {
                "inner" : {
                  "name" : "abc"
                }
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
