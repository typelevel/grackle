// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import edu.gemini.grackle._

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlPaging1Suite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("paging (initial)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          offset
          limit
          total
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
            "offset" : 0,
            "limit" : 3,
            "total" : 239,
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("paging (offset)") {
    val query = """
      query {
        countries(offset: 2, limit: 3) {
          offset
          limit
          total
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
            "offset" : 2,
            "limit" : 3,
            "total" : 239,
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("paging (final, over)") {
    val query = """
      query {
        countries(offset: 237, limit: 3) {
          offset
          limit
          total
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
            "offset" : 237,
            "limit" : 3,
            "total" : 239,
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("paging (final, exact)") {
    val query = """
      query {
        countries(offset: 236, limit: 3) {
          offset
          limit
          total
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
            "offset" : 236,
            "limit" : 3,
            "total" : 239,
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("paging (near end, more)") {
    val query = """
      query {
        countries(offset: 235, limit: 3) {
          offset
          limit
          total
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
            "offset" : 235,
            "limit" : 3,
            "total" : 239,
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("paging (only bounds, has more)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          offset
          limit
          total
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "offset" : 0,
            "limit" : 3,
            "total" : 239
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("paging (only bounds, no more)") {
    val query = """
      query {
        countries(offset: 236, limit: 3) {
          offset
          limit
          total
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "offset" : 236,
            "limit" : 3,
            "total" : 239
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("paging (aliasing)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          foo:offset
          bar:limit
          baz:total
          quux:items {
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
            "foo" : 0,
            "bar" : 3,
            "baz" : 239,
            "quux" : [
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("paging (introspection)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          __typename
          offset
          limit
          total
          items {
            __typename
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
            "__typename" : "PagedCountry",
            "offset" : 0,
            "limit" : 3,
            "total" : 239,
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
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("paging (introspection, no items)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          __typename
          offset
          limit
          total
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "__typename" : "PagedCountry",
            "offset" : 0,
            "limit" : 3,
            "total" : 239
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("paging (nested)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          offset
          limit
          total
          items {
            code
            name
            cities(offset: 0, limit: 2) {
              offset
              limit
              total
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
            "offset" : 0,
            "limit" : 3,
            "total" : 239,
            "items" : [
              {
                "code" : "ABW",
                "name" : "Aruba",
                "cities" : {
                  "offset" : 0,
                  "limit" : 2,
                  "total" : 1,
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
                  "offset" : 0,
                  "limit" : 2,
                  "total" : 4,
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
                  "offset" : 0,
                  "limit" : 2,
                  "total" : 5,
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("paging (nested, only bounds)") {
    val query = """
      query {
        countries(offset: 0, limit: 3) {
          offset
          limit
          total
          items {
            code
            name
            cities(offset: 0, limit: 2) {
              offset
              limit
              total
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : {
            "offset" : 0,
            "limit" : 3,
            "total" : 239,
            "items" : [
              {
                "code" : "ABW",
                "name" : "Aruba",
                "cities" : {
                  "offset" : 0,
                  "limit" : 2,
                  "total" : 1
                }
              },
              {
                "code" : "AFG",
                "name" : "Afghanistan",
                "cities" : {
                  "offset" : 0,
                  "limit" : 2,
                  "total" : 4
                }
              },
              {
                "code" : "AGO",
                "name" : "Angola",
                "cities" : {
                  "offset" : 0,
                  "limit" : 2,
                  "total" : 5
                }
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
