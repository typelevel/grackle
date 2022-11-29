// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import cats.effect.unsafe.implicits.global

import edu.gemini.grackle._
import syntax._

import grackle.test.GraphQLResponseTests.assertWeaklyEqual

trait SqlCompositeKeySpec extends AnyFunSuite {
  def mapping: QueryExecutor[IO, Json]

  test("root query") {
    val query = """
      query {
        parents {
          key1
          key2
          children {
            id
            parent1
            parent2
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "parents" : [
            {
              "key1" : 2,
              "key2" : "bar",
              "children" : [
                {
                  "id" : 4,
                  "parent1" : 2,
                  "parent2" : "bar"
                }
              ]
            },
            {
              "key1" : 2,
              "key2" : "foo",
              "children" : [
                {
                  "id" : 3,
                  "parent1" : 2,
                  "parent2" : "foo"
                }
              ]
            },
            {
              "key1" : 1,
              "key2" : "bar",
              "children" : [
                {
                  "id" : 2,
                  "parent1" : 1,
                  "parent2" : "bar"
                }
              ]
            },
            {
              "key1" : 1,
              "key2" : "foo",
              "children" : [
                {
                  "id" : 1,
                  "parent1" : 1,
                  "parent2" : "foo"
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }
}
