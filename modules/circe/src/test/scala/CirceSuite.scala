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

package grackle
package circetests

import io.circe.literal._
import munit.CatsEffectSuite

final class CirceSuite extends CatsEffectSuite {
  test("scalars") {
    val query = """
      query {
        root {
          bool
          int
          float
          string
          bigDecimal
          id
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : {
            "bool" : true,
            "int" : 23,
            "float": 1.3,
            "string": "foo",
            "bigDecimal": 1.2,
            "id": "bar"
          }
        }
      }
    """

    val res = TestCirceMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("enums") {
    val query = """
      query {
        root {
          choice
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : {
            "choice": "ONE"
          }
        }
      }
    """

    val res = TestCirceMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("objects") {
    val query = """
      query {
        root {
          object {
            id
            aField
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : {
            "object" : {
              "id" : "obj",
              "aField" : 27
            }
          }
        }
      }
    """

    val res = TestCirceMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("arrays") {
    val query = """
      query {
        root {
          children {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : {
            "children" : [
              {
                "id" : "a"
              },
              {
                "id" : "b"
              }
            ]
          }
        }
      }
    """

    val res = TestCirceMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("fragments") {
    val query = """
      query {
        root {
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
    """

    val expected = json"""
      {
        "data" : {
          "root" : {
            "children" : [
              {
                "id" : "a",
                "aField" : 11
              },
              {
                "id" : "b",
                "bField" : "quux"
              }
            ]
          }
        }
      }
    """

    val res = TestCirceMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("supertype fragment") {
    val query = """
      query {
        root {
          object {
            ... on Child {
              id
            }
            aField
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : {
            "object" : {
              "id" : "obj",
              "aField" : 27
            }
          }
        }
      }
    """

    val res = TestCirceMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("introspection") {
    val query = """
      query {
        root {
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
    """

    val expected = json"""
      {
        "data" : {
          "root" : {
            "object" : {
              "__typename" : "A",
              "id" : "obj"
            },
            "children" : [
              {
                "__typename" : "A",
                "id" : "a"
              },
              {
                "__typename" : "B",
                "id" : "b"
              }
            ]
          }
        }
      }
    """

    val res = TestCirceMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("count") {
    val query = """
      query {
        root {
          numChildren
          children {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : {
            "numChildren" : 2,
            "children" : [
              {
                "id" : "a"
              },
              {
                "id" : "b"
              }
            ]
          }
        }
      }
    """

    val res = TestCirceMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("hidden") {
    val query = """
      query {
        root {
          hidden
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "No field 'hidden' for type Root"
          }
        ]
      }
    """

    val res = TestCirceMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("computed") {
    val query = """
      query {
        root {
          computed
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : {
            "computed" : 14
          }
        }
      }
    """

    val res = TestCirceMapping.compileAndRun(query)

    assertIO(res, expected)
  }
}
