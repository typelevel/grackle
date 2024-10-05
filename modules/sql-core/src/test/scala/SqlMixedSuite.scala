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

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlMixedSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("DB query") {
    val query = """
      query {
        movie(id: "6a7837fc-b463-4d32-b628-0f4b3065cb21") {
          title
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "movie" : {
            "title" : "Celine et Julie Vont en Bateau"
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("value query") {
    val query = """
      query {
        foo {
          value
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "foo" : {
            "value" : 23
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("circe query") {
    val query = """
      query {
        bar {
          message
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "bar" : {
            "message" : "Hello world"
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("mixed query") {
    val query = """
      query {
        movie(id: "6a7837fc-b463-4d32-b628-0f4b3065cb21") {
          title
        }
        foo {
          value
        }
        bar {
          message
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "movie" : {
            "title" : "Celine et Julie Vont en Bateau"
          },
          "foo" : {
            "value" : 23
          },
          "bar" : {
            "message" : "Hello world"
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("mixed query nested (1)") {
    val query = """
      query {
        movie(id: "6a7837fc-b463-4d32-b628-0f4b3065cb21") {
          title
          nested {
            message
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "movie" : {
            "title" : "Celine et Julie Vont en Bateau",
            "nested" : {
               "message": "Hello world nested"
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("mixed query nested (2)") {
    val query = """
      query {
        movie(id: "6a7837fc-b463-4d32-b628-0f4b3065cb21") {
          nested {
            message
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "movie" : {
            "nested" : {
               "message": "Hello world nested"
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("mixed query nested (3)") {
    val query = """
      query {
        movies {
          rating
          nested {
            message
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "movies" : [
            {
              "rating" : 7.8,
              "nested" : {
                "message" : "Hello world nested"
              }
            },
            {
              "rating" : 7.8,
              "nested" : {
                "message" : "Hello world nested"
              }
            },
            {
              "rating" : 7.8,
              "nested" : {
                "message" : "Hello world nested"
              }
            },
            {
              "rating" : 7.8,
              "nested" : {
                "message" : "Hello world nested"
              }
            },
            {
              "rating" : 7.8,
              "nested" : {
                "message" : "Hello world nested"
              }
            },
            {
              "rating" : 7.8,
              "nested" : {
                "message" : "Hello world nested"
              }
            },
            {
              "rating" : 7.8,
              "nested" : {
                "message" : "Hello world nested"
              }
            },
            {
              "rating" : 7.8,
              "nested" : {
                "message" : "Hello world nested"
              }
            },
            {
              "rating" : 7.8,
              "nested" : {
                "message" : "Hello world nested"
              }
            },
            {
              "rating" : 7.8,
              "nested" : {
                "message" : "Hello world nested"
              }
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("mixed query only leaf (1)") {
    val query = """
      query {
        movies {
          genre
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "movies" : [
            {
              "genre" : "comedy"
            },
            {
              "genre" : "comedy"
            },
            {
              "genre" : "comedy"
            },
            {
              "genre" : "comedy"
            },
            {
              "genre" : "comedy"
            },
            {
              "genre" : "comedy"
            },
            {
              "genre" : "comedy"
            },
            {
              "genre" : "comedy"
            },
            {
              "genre" : "comedy"
            },
            {
              "genre" : "comedy"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("mixed query only leaf (2)") {
    val query = """
      query {
        movies {
          rating
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "movies" : [
            {
              "rating" : 7.8
            },
            {
              "rating" : 7.8
            },
            {
              "rating" : 7.8
            },
            {
              "rating" : 7.8
            },
            {
              "rating" : 7.8
            },
            {
              "rating" : 7.8
            },
            {
              "rating" : 7.8
            },
            {
              "rating" : 7.8
            },
            {
              "rating" : 7.8
            },
            {
              "rating" : 7.8
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
