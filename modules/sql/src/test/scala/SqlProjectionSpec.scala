// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import edu.gemini.grackle.syntax._

import org.scalatest.funsuite.AnyFunSuite
import edu.gemini.grackle.QueryExecutor
import cats.effect.IO
import io.circe.Json
import cats.effect.unsafe.implicits.global

trait SqlProjectionSpec extends AnyFunSuite {
  def mapping: QueryExecutor[IO, Json]

  ignore("projected query with nested query") {
    val query = """
      query {
        level0(filter: { attr: true }) {
          id
          level1 {
            id
            level2 {
              id
              attr
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "level0" : [
            {
              "id" : "01",
              "level1" : [
                {
                  "id" : "12",
                  "level2" : [
                    {
                      "id" : "24",
                      "attr" : true
                    },
                    {
                      "id" : "25",
                      "attr" : false
                    }
                  ]
                },
                {
                  "id" : "13",
                  "level2" : [
                    {
                      "id" : "26",
                      "attr" : false
                    },
                    {
                      "id" : "27",
                      "attr" : false
                    }
                  ]
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assert(res == expected)
  }

  ignore("projected query with nested filtered queries (1)") {
    val query = """
      query {
        level0(filter: { attr: true }) {
          id
          level1 {
            id
            level2(filter: { attr: true }) {
              id
              attr
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "level0" : [
            {
              "id" : "01",
              "level1" : [
                {
                  "id" : "12",
                  "level2" : [
                    {
                      "id" : "24",
                      "attr" : true
                    }
                  ]
                },
                {
                  "id" : "13",
                  "level2" : [
                  ]
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assert(res == expected)
  }
}
