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

trait SqlPaging2Spec extends AnyFunSuite {
  def mapping: QueryExecutor[IO, Json]

  test("paging (initial)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          items {
            code
            name
          }
          hasMore
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "hasMore" : true,
            "items" : [
              {
                "code" : "ABW",
                "name" : "Aruba"
              },
              {
                "code" : "AFG",
                "name" : "Afghanistan"
              },
              {
                "code" : "AGO",
                "name" : "Angola"
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

  test("paging (offset)") {
    val query = """
      query {
        countries(offset: 2, limit: 3) {
          items {
            code
            name
          }
          hasMore
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "hasMore" : true,
            "items" : [
              {
                "code" : "AGO",
                "name" : "Angola"
              },
              {
                "code" : "AIA",
                "name" : "Anguilla"
              },
              {
                "code" : "ALB",
                "name" : "Albania"
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

  test("paging (final, over)") {
    val query = """
      query {
        countries(offset: 237, limit: 3) {
          items {
            code
            name
          }
          hasMore
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "hasMore" : false,
            "items" : [
              {
                "code" : "ZMB",
                "name" : "Zambia"
              },
              {
                "code" : "ZWE",
                "name" : "Zimbabwe"
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

  test("paging (final, exact)") {
    val query = """
      query {
        countries(offset: 236, limit: 3) {
          items {
            code
            name
          }
          hasMore
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "hasMore" : false,
            "items" : [
              {
                "code" : "ZAF",
                "name" : "South Africa"
              },
              {
                "code" : "ZMB",
                "name" : "Zambia"
              },
              {
                "code" : "ZWE",
                "name" : "Zimbabwe"
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

  test("paging (near end, more)") {
    val query = """
      query {
        countries(offset: 235, limit: 3) {
          items {
            code
            name
          }
          hasMore
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "hasMore" : true,
            "items" : [
              {
                "code" : "YUG",
                "name" : "Yugoslavia"
              },
              {
                "code" : "ZAF",
                "name" : "South Africa"
              },
              {
                "code" : "ZMB",
                "name" : "Zambia"
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

  test("paging (only items)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          items {
            code
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "items" : [
              {
                "code" : "ABW",
                "name" : "Aruba"
              },
              {
                "code" : "AFG",
                "name" : "Afghanistan"
              },
              {
                "code" : "AGO",
                "name" : "Angola"
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

  test("paging (only has more, has more)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          hasMore
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "hasMore" : true
          }
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("paging (only has more, no more)") {
    val query = """
      query {
        countries(offset: 236, limit: 3) {
          hasMore
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "hasMore" : false
          }
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("paging (aliasing)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          foo: items {
            code
            name
          }
          bar: hasMore
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "foo" : [
              {
                "code" : "ABW",
                "name" : "Aruba"
              },
              {
                "code" : "AFG",
                "name" : "Afghanistan"
              },
              {
                "code" : "AGO",
                "name" : "Angola"
              }
            ],
            "bar" : true
          }
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("paging (introspection)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          __typename
          items {
            __typename
            code
            name
          }
          hasMore
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "__typename" : "PagedCountry",
            "items" : [
              {
                "__typename" : "Country",
                "code" : "ABW",
                "name" : "Aruba"
              },
              {
                "__typename" : "Country",
                "code" : "AFG",
                "name" : "Afghanistan"
              },
              {
                "__typename" : "Country",
                "code" : "AGO",
                "name" : "Angola"
              }
            ],
            "hasMore" : true
          }
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("paging (introspection, no items)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          __typename
          hasMore
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "__typename" : "PagedCountry",
            "hasMore" : true
          }
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("paging (only introspection)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          __typename
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "__typename" : "PagedCountry"
          }
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("paging (nested)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          items {
            code
            name
            cities(offset: 0, limit: 2) {
              items {
                name
              }
              hasMore
            }
          }
          hasMore
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "items" : [
              {
                "code" : "ABW",
                "name" : "Aruba",
                "cities" : {
                  "items" : [
                    {
                      "name" : "Oranjestad"
                    }
                  ],
                  "hasMore" : false
                }
              },
              {
                "code" : "AFG",
                "name" : "Afghanistan",
                "cities" : {
                  "items" : [
                    {
                      "name" : "Herat"
                    },
                    {
                      "name" : "Kabul"
                    }
                  ],
                  "hasMore" : true
                }
              },
              {
                "code" : "AGO",
                "name" : "Angola",
                "cities" : {
                  "items" : [
                    {
                      "name" : "Benguela"
                    },
                    {
                      "name" : "Huambo"
                    }
                  ],
                  "hasMore" : true
                }
              }
            ],
            "hasMore" : true
          }
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("paging (nested, only items)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          items {
            code
            name
            cities(offset: 0, limit: 2) {
              items {
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
          "countries" : {
            "items" : [
              {
                "code" : "ABW",
                "name" : "Aruba",
                "cities" : {
                  "items" : [
                    {
                      "name" : "Oranjestad"
                    }
                  ]
                }
              },
              {
                "code" : "AFG",
                "name" : "Afghanistan",
                "cities" : {
                  "items" : [
                    {
                      "name" : "Herat"
                    },
                    {
                      "name" : "Kabul"
                    }
                  ]
                }
              },
              {
                "code" : "AGO",
                "name" : "Angola",
                "cities" : {
                  "items" : [
                    {
                      "name" : "Benguela"
                    },
                    {
                      "name" : "Huambo"
                    }
                  ]
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

  test("paging (nested, only has more)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          items {
            code
            name
            cities(offset: 0, limit: 2) {
              hasMore
            }
          }
          hasMore
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "items" : [
              {
                "code" : "ABW",
                "name" : "Aruba",
                "cities" : {
                  "hasMore" : false
                }
              },
              {
                "code" : "AFG",
                "name" : "Afghanistan",
                "cities" : {
                  "hasMore" : true
                }
              },
              {
                "code" : "AGO",
                "name" : "Angola",
                "cities" : {
                  "hasMore" : true
                }
              }
            ],
            "hasMore" : true
          }
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }
}
