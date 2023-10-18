// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package grackle.sql.test

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._

import grackle.test.GraphQLResponseTests.{assertWeaklyEqual, assertWeaklyEqualIO}

trait SqlNestedEffectsSuite extends CatsEffectSuite {
  def mapping: IO[(CurrencyService[IO], Mapping[IO])]

  test("simple effectful service call") {
    val expected = json"""
      [
        {
          "code" : "EUR",
          "exchangeRate" : 1.12,
          "countryCode" : "NL"
        },
        {
          "code" : "GBP",
          "exchangeRate" : 1.25,
          "countryCode" : "GB"
        }
      ]
    """

    val res = mapping.flatMap(_._1.get(List("GB", "NL")))

    assertWeaklyEqualIO(res, expected)
  }

  test("simple composed query (1)") {
    val query = """
      query {
        country(code: "GBR") {
          currencies {
            code
            exchangeRate
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "currencies": [
              {
                "code": "GBP",
                "exchangeRate": 1.25
              }
            ]
          }
        }
      }
    """

    val res = mapping.flatMap(_._2.compileAndRun(query))

    assertWeaklyEqualIO(res, expected)
  }

  test("simple composed query (2)") {
    val query = """
      query {
        country(code: "GBR") {
          name
          currencies {
            code
            exchangeRate
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "United Kingdom",
            "currencies": [
              {
                "code": "GBP",
                "exchangeRate": 1.25
              }
            ]
          }
        }
      }
    """

    val res = mapping.flatMap(_._2.compileAndRun(query))

    assertWeaklyEqualIO(res, expected)
  }

  test("composed query with aliasing") {
    val query = """
      query {
        country(code: "GBR") {
          name
          cur1:currencies {
            code
          }
          cur2:currencies {
            exchangeRate
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "United Kingdom",
            "cur1" : [
              {
                "code" : "GBP"
              }
            ],
            "cur2" : [
              {
                "exchangeRate" : 1.25
              }
            ]
          }
        }
      }
    """

    val res = mapping.flatMap(_._2.compileAndRun(query))

    assertWeaklyEqualIO(res, expected)
  }

  test("simple multiple nested query") {
    val query = """
      query {
        cities(namePattern: "Ame%") {
          name
          country {
            name
            currencies {
              code
              exchangeRate
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data": {
          "cities" : [
            {
              "name" : "Amersfoort",
              "country" : {
                "name" : "Netherlands",
                "currencies" : [
                  {
                    "code" : "EUR",
                    "exchangeRate" : 1.12
                  }
                ]
              }
            },
            {
              "name" : "Americana",
              "country" : {
                "name" : "Brazil",
                "currencies" : [
                  {
                    "code" : "BRL",
                    "exchangeRate" : 0.25
                  }
                ]
              }
            }
          ]
        }
      }
    """

    val prg =
      (for {
        cm  <- mapping
        m   =  cm._2
        n0  <- cm._1.count
        res <- m.compileAndRun(query)
        n1  <- cm._1.count
      } yield (res, n0, n1))

    prg.map { case (res, n0, n1) =>
      assert(n0 == 0 && n1 == 1)
      assertWeaklyEqual(res, expected)
    }
  }

  test("simple query with mutation") {
    val query = """
      query {
        cities(namePattern: "Ame%") {
          name
          country {
            name
            currencies {
              code
              exchangeRate
            }
          }
        }
      }
    """

    val expected0 = json"""
      {
        "data": {
          "cities" : [
            {
              "name" : "Amersfoort",
              "country" : {
                "name" : "Netherlands",
                "currencies" : [
                  {
                    "code" : "EUR",
                    "exchangeRate" : 1.12
                  }
                ]
              }
            },
            {
              "name" : "Americana",
              "country" : {
                "name" : "Brazil",
                "currencies" : [
                  {
                    "code" : "BRL",
                    "exchangeRate" : 0.25
                  }
                ]
              }
            }
          ]
        }
      }
    """

    val expected1 = json"""
      {
        "data": {
          "cities" : [
            {
              "name" : "Amersfoort",
              "country" : {
                "name" : "Netherlands",
                "currencies" : [
                  {
                    "code" : "EUR",
                    "exchangeRate" : 1.13
                  }
                ]
              }
            },
            {
              "name" : "Americana",
              "country" : {
                "name" : "Brazil",
                "currencies" : [
                  {
                    "code" : "BRL",
                    "exchangeRate" : 0.25
                  }
                ]
              }
            }
          ]
        }
      }
    """

    val prg =
      (for {
        cm   <- mapping
        c    =  cm._1
        m    =  cm._2
        n0   <- cm._1.count
        res0 <- m.compileAndRun(query)
        n1   <- cm._1.count
        _    <- c.update("EUR", 1.13)
        res1 <- m.compileAndRun(query)
        n2   <- cm._1.count
      } yield (res0, res1, n0, n1, n2))

    prg.map { case (res0, res1, n0, n1, n2) =>
      assert(n0 == 0 && n1 == 1 && n2 == 2)

      assertWeaklyEqual(res0, expected0)
      assertWeaklyEqual(res1, expected1)
    }
  }

  test("composed query with introspection") {
    val query = """
      query {
        country(code: "GBR") {
          __typename
          name
          currencies {
            __typename
            code
            exchangeRate
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "__typename" : "Country",
            "name" : "United Kingdom",
            "currencies" : [
              {
                "__typename" : "Currency",
                "code" : "GBP",
                "exchangeRate" : 1.25
              }
            ]
          }
        }
      }
    """

    val res = mapping.flatMap(_._2.compileAndRun(query))

    assertWeaklyEqualIO(res, expected)
  }

  test("doubly composed query (1)") {
    val query = """
      query {
        country(code: "GBR") {
          name
          currencies {
            code
            exchangeRate
            country {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "United Kingdom",
            "currencies" : [
              {
                "code" : "GBP",
                "exchangeRate" : 1.25,
                "country" : {
                  "name" : "United Kingdom"
                }
              }
            ]
          }
        }
      }
    """

    val res = mapping.flatMap(_._2.compileAndRun(query))

    assertWeaklyEqualIO(res, expected)
  }

  test("doubly composed query (2)") {
    val query = """
      query {
        cities(namePattern: "Ame%") {
          name
          country {
            name
            currencies {
              code
              exchangeRate
              country {
                name
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "cities" : [
            {
              "name" : "Amersfoort",
              "country" : {
                "name" : "Netherlands",
                "currencies" : [
                  {
                    "code" : "EUR",
                    "exchangeRate" : 1.12,
                    "country" : {
                      "name" : "Netherlands"
                    }
                  }
                ]
              }
            },
            {
              "name" : "Americana",
              "country" : {
                "name" : "Brazil",
                "currencies" : [
                  {
                    "code" : "BRL",
                    "exchangeRate" : 0.25,
                    "country" : {
                      "name" : "Brazil"
                    }
                  }
                ]
              }
            }
          ]
        }
      }
    """

    val res = mapping.flatMap(_._2.compileAndRun(query))

    assertWeaklyEqualIO(res, expected)
  }

  test("doubly composed query with alias") {
    val query = """
      query {
        country(code: "GBR") {
          name
          currencies {
            code
            exchangeRate
            foo:country {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "United Kingdom",
            "currencies" : [
              {
                "code" : "GBP",
                "exchangeRate" : 1.25,
                "foo" : {
                  "name" : "United Kingdom"
                }
              }
            ]
          }
        }
      }
    """

    val res = mapping.flatMap(_._2.compileAndRun(query))

    assertWeaklyEqualIO(res, expected)
  }

}
