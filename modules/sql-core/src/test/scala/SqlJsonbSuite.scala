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

trait SqlJsonbSuite extends CatsEffectSuite {

  def mapping: Mapping[IO]

  test("simple jsonb query") {
    val query = """
      query {
        record(id: 1) {
          id
          record {
            bool
            int
            float
            string
            id
            string
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "record" : {
            "id" : 1,
            "record" : {
              "bool" : true,
              "int" : 1,
              "float" : 1.3,
              "string" : "foo",
              "id" : "Foo"
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("enums") {
    val query = """
      query {
        records {
          record {
            choice
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "records" : [
            {
              "record" : {
                "choice" : "ONE"
              }
            },
            {
              "record" : {
                "choice" : "TWO"
              }
            },
            {
              "record" : {
                "choice" : "THREE"
              }
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("objects") {
    val query = """
      query {
        record(id: 1) {
          record {
            object {
              id
              aField
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "record" : {
            "record" : {
              "object" : {
                "id" : "obj0",
                "aField" : 27
              }
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("arrays") {
    val query = """
      query {
        records {
          record {
            children {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "records" : [
            {
              "record" : {
                "children" : [
                  {
                    "id" : "a0"
                  },
                  {
                    "id" : "b0"
                  }
                ]
              }
            },
            {
              "record" : {
                "children" : [
                  {
                    "id" : "a1"
                  },
                  {
                    "id" : "b1"
                  }
                ]
              }
            },
            {
              "record" : {
                "children" : [
                  {
                    "id" : "a2"
                  },
                  {
                    "id" : "b2"
                  }
                ]
              }
            }
          ]
        }
      }
      """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("fragments") {
    val query = """
      query {
        record(id: 1) {
          record {
            children {
              id
              ... on A {
                aField
              }
              ... on B {
                bField
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "record" : {
            "record" : {
              "children" : [
                {
                  "id" : "a0",
                  "aField" : 11
                },
                {
                  "id" : "b0",
                  "bField" : "wibble"
                }
              ]
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("introspection") {
    val query = """
      query {
        record(id: 1) {
          record {
            object {
              __typename
              id
            }
            children {
              __typename
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "record" : {
            "record" : {
              "object" : {
                "__typename" : "A",
                "id" : "obj0"
              },
              "children" : [
                {
                  "__typename" : "A",
                  "id" : "a0"
                },
                {
                  "__typename" : "B",
                  "id" : "b0"
                }
              ]
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
