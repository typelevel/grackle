// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import grackle.test.GraphQLResponseTests.assertWeaklyEqual
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

trait SqlArrayJoinSpec extends AnyFunSuite {
  def mapping: QueryExecutor[IO, Json]

  test("base query") {
    val query = """
      query {
        root {
          listA {
            id
            elemA
          }
          listB {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a0",
                  "elemA" : ["foo1","foo2"]
                },
                {
                  "id" : "a1",
                  "elemA" : ["bar1","bar2"]
                },
                {
                  "id" : "a2",
                  "elemA" : ["baz1","baz2"]
                },
                {
                  "id" : "a3",
                  "elemA" : ["quux1","quux2"]
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                },
                {
                  "id" : "b3",
                  "elemB" : 11
                }
              ]
            },
            {
              "listA" : [
                {
                  "id" : "a4",
                  "elemA" : ["foo11","foo22"]
                },
                {
                  "id" : "a5",
                  "elemA" : ["bar11","bar22"]
                },
                {
                  "id" : "a6",
                  "elemA" : ["baz11","baz22"]
                },
                {
                  "id" : "a7",
                  "elemA" : ["quux11","quux22"]
                }
              ],
              "listB" : [
                {
                  "id" : "b4",
                  "elemB" : 231
                },
                {
                  "id" : "b5",
                  "elemB" : 131
                },
                {
                  "id" : "b6",
                  "elemB" : 171
                },
                {
                  "id" : "b7",
                  "elemB" : 111
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
