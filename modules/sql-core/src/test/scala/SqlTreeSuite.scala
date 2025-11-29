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

trait SqlTreeSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("root query") {
    val query = """
      query {
        bintree(id: 0) {
          id
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "bintree" : {
            "id" : 0
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("child query (1)") {
    val query = """
      query {
        bintree(id: 0) {
          id
          left {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "bintree" : {
            "id" : 0,
            "left" : {
              "id" : 1
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("child query (2)") {
    val query = """
      query {
        bintree(id: 0) {
          id
          left {
            id
          }
          right {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "bintree" : {
            "id" : 0,
            "left" : {
              "id" : 1
            },
            "right" : {
              "id" : 2
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("child query (3)") {
    val query = """
      query {
        bintree(id: 0) {
          id
          left {
            id
            left {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "bintree" : {
            "id" : 0,
            "left" : {
              "id" : 1,
              "left" : {
                "id" : 3
              }
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("child query (4)") {
    val query = """
      query {
        bintree(id: 0) {
          id
          left {
            id
            left {
              id
            }
            right {
              id
            }
          }
          right {
            id
            left {
              id
            }
            right {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "bintree" : {
            "id" : 0,
            "left" : {
              "id" : 1,
              "left" : {
                "id" : 3
              },
              "right" : {
                "id" : 4
              }
            },
            "right" : {
              "id" : 2,
              "left" : {
                "id" : 5
              },
              "right" : {
                "id" : 6
              }
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
