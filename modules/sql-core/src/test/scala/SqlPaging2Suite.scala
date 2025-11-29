// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlPaging2Suite extends CatsEffectSuite {
  def mapping: Mapping[IO]

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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
