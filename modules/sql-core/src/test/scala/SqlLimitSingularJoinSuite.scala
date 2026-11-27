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

trait SqlLimitSingularJoinSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("limited roots with a singular field joined through an association table") {
    val query = """
      query {
        roots(limit: 2) {
          id
          name
          firstTarget {
            id
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "roots" : [
            {
              "id" : "r0",
              "name" : "root0",
              "firstTarget" : {
                "id" : "t0",
                "name" : "target0"
              }
            },
            {
              "id" : "r1",
              "name" : "root1",
              "firstTarget" : {
                "id" : "t2",
                "name" : "target2"
              }
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("limited roots including one without an association row") {
    val query = """
      query {
        roots(limit: 3) {
          id
          firstTarget {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "roots" : [
            {
              "id" : "r0",
              "firstTarget" : {
                "id" : "t0"
              }
            },
            {
              "id" : "r1",
              "firstTarget" : {
                "id" : "t2"
              }
            },
            {
              "id" : "r2",
              "firstTarget" : null
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("limited union of embedded lists and a nullable scalar with a singular joined field") {
    val query = """
      query {
        roots(limit: 2) {
          id
          name
          lists {
            listA {
              id
            }
            listB {
              id
            }
          }
          firstTarget {
            id
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "roots" : [
            {
              "id" : "r0",
              "name" : "root0",
              "lists" : {
                "listA" : [
                  {
                    "id" : "a00"
                  },
                  {
                    "id" : "a01"
                  }
                ],
                "listB" : [
                  {
                    "id" : "b00"
                  },
                  {
                    "id" : "b01"
                  }
                ]
              },
              "firstTarget" : {
                "id" : "t0",
                "name" : "target0"
              }
            },
            {
              "id" : "r1",
              "name" : "root1",
              "lists" : {
                "listA" : [
                  {
                    "id" : "a10"
                  },
                  {
                    "id" : "a11"
                  }
                ],
                "listB" : [
                  {
                    "id" : "b10"
                  },
                  {
                    "id" : "b11"
                  }
                ]
              },
              "firstTarget" : {
                "id" : "t2",
                "name" : "target2"
              }
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("limited union of embedded lists and a non-null scalar with a singular joined field") {
    val query = """
      query {
        roots(limit: 2) {
          id
          code
          lists {
            listA {
              id
            }
            listB {
              id
            }
          }
          firstTarget {
            id
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "roots" : [
            {
              "id" : "r0",
              "code" : "c0",
              "lists" : {
                "listA" : [
                  {
                    "id" : "a00"
                  },
                  {
                    "id" : "a01"
                  }
                ],
                "listB" : [
                  {
                    "id" : "b00"
                  },
                  {
                    "id" : "b01"
                  }
                ]
              },
              "firstTarget" : {
                "id" : "t0",
                "name" : "target0"
              }
            },
            {
              "id" : "r1",
              "code" : "c1",
              "lists" : {
                "listA" : [
                  {
                    "id" : "a10"
                  },
                  {
                    "id" : "a11"
                  }
                ],
                "listB" : [
                  {
                    "id" : "b10"
                  },
                  {
                    "id" : "b11"
                  }
                ]
              },
              "firstTarget" : {
                "id" : "t2",
                "name" : "target2"
              }
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }
}
