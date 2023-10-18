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

package composed

import io.circe.literal._
import munit.CatsEffectSuite

final class ComposedSuite extends CatsEffectSuite {
  test("simple currency query") {
    val query = """
      query {
        fx(code: "GBP") {
          code
          exchangeRate
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "fx": {
            "code": "GBP",
            "exchangeRate": 1.25
          }
        }
      }
    """

    val res = ComposedMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("simple country query") {
    val query = """
      query {
        country(code: "GBR") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country": {
            "name": "United Kingdom"
          }
        }
      }
    """

    val res = ComposedMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("simple nested query") {
    val query = """
      query {
        country(code: "GBR") {
          name
          currency {
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
            "currency": {
              "code": "GBP",
              "exchangeRate": 1.25
            }
          }
        }
      }
    """

    val res = ComposedMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("simple multiple nested query") {
    val query = """
      query {
        countries {
          name
          currency {
            code
            exchangeRate
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : [
            {
              "name" : "Germany",
              "currency" : {
                "code" : "EUR",
                "exchangeRate" : 1.12
              }
            },
            {
              "name" : "France",
              "currency" : {
                "code" : "EUR",
                "exchangeRate" : 1.12
              }
            },
            {
              "name" : "United Kingdom",
              "currency" : {
                "code" : "GBP",
                "exchangeRate" : 1.25
              }
            }
          ]
        }
      }
    """

    val res = ComposedMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("multiple aliased root queries") {
    val query = """
      query {
        gbr: country(code: "GBR") {
          name
          currency {
            code
          }
        }
        fra: country(code: "FRA") {
          name
          currency {
            code
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "gbr" : {
            "name" : "United Kingdom",
            "currency" : {
              "code" : "GBP"
            }
          },
          "fra" : {
            "name" : "France",
            "currency" : {
              "code" : "EUR"
            }
          }
        }
      }
    """

    val res = ComposedMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("nested aliased query") {
    val query = """
      query {
        country(code: "GBR") {
          name
          fx: currency {
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
            "fx": {
              "code": "GBP",
              "exchangeRate": 1.25
            }
          }
        }
      }
    """

    val res = ComposedMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("composed query with introspection") {
    val query = """
      query {
        country(code: "GBR") {
          __typename
          name
          currency {
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
            "currency" : {
              "__typename" : "Currency",
              "code" : "GBP",
              "exchangeRate" : 1.25
            }
          }
        }
      }
    """

    val res = ComposedMapping.compileAndRun(query)

    assertIO(res, expected)
  }
}
