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

trait SqlNestedEffectsSpec extends AnyFunSuite {
  def mapping: IO[(CurrencyService[IO], QueryExecutor[IO, Json])]

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

    val res = mapping.flatMap(_._1.get(List("GB", "NL"))).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("simple composed query") {
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

    val res = mapping.flatMap(_._2.compileAndRun(query)).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
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

    val res = mapping.flatMap(_._2.compileAndRun(query)).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
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

    val (res, n0, n1) =
      (for {
        cm  <- mapping
        c   =  cm._1
        m   =  cm._2
        n0  <- cm._1.count
        res <- m.compileAndRun(query)
        n1  <- cm._1.count
      } yield (res, n0, n1)).unsafeRunSync()

    //println(res)

    assert(n0 == 0 && n1 == 1)

    assertWeaklyEqual(res, expected)
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

    val (res0, res1, n0, n1, n2) =
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
      } yield (res0, res1, n0, n1, n2)).unsafeRunSync()

    //println(res0)
    //println(res1)

    assert(n0 == 0 && n1 == 1 && n2 == 2)

    assertWeaklyEqual(res0, expected0)
    assertWeaklyEqual(res1, expected1)
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

    val res = mapping.flatMap(_._2.compileAndRun(query)).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
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

    val res = mapping.flatMap(_._2.compileAndRun(query)).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
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

    val res = mapping.flatMap(_._2.compileAndRun(query)).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
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

    val res = mapping.flatMap(_._2.compileAndRun(query)).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

}
