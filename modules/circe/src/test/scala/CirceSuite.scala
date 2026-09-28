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

package grackle.circetests

import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

import grackle.{Env, Path, Predicate}
import grackle.PathTerm.UniquePath
import grackle.Predicate.{Const, Eql}

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

  test("booleans and numbers are coerced to String") {
    val query = """
      query {
        int
        float
        bool
        string
      }
    """

    val expected = json"""
      {
        "data" : {
          "int" : "42",
          "float" : "1.5",
          "bool" : "true",
          "string" : "foo"
        }
      }
    """

    val res = TestCirceScalarCoercionMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("lists and objects are not coerced to String") {
    val query = """
      query {
        array
        object
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Cannot coerce JSON Array value '[1]' to type String",
            "locations" : [ { "line" : 3, "column" : 9 } ],
            "path" : [ "array" ]
          },
          {
            "message" : "Cannot coerce JSON Object value '{\"a\":1}' to type String",
            "locations" : [ { "line" : 4, "column" : 9 } ],
            "path" : [ "object" ]
          }
        ],
        "data" : {
          "array" : null,
          "object" : null
        }
      }
    """

    val res = TestCirceScalarCoercionMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("large Int values and strings are coerced to Int, Float, Boolean and ID") {
    val query = """
      query {
        bigInt
        intFromString
        floatFromString
        boolFromString
        idFromInt
      }
    """

    val expected = json"""
      {
        "data" : {
          "bigInt" : 3000000000,
          "intFromString" : 42,
          "floatFromString" : 1.5,
          "boolFromString" : true,
          "idFromInt" : "23"
        }
      }
    """

    val res = TestCirceScalarCoercionMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("values that do not coerce to Int, Float or Boolean are errors") {
    val query = """
      query {
        badInt
        badFloat
        badBool
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Cannot coerce JSON String value '\"foo\"' to type Int",
            "locations" : [ { "line" : 3, "column" : 9 } ],
            "path" : [ "badInt" ]
          },
          {
            "message" : "Cannot coerce JSON Boolean value 'true' to type Float",
            "locations" : [ { "line" : 4, "column" : 9 } ],
            "path" : [ "badFloat" ]
          },
          {
            "message" : "Cannot coerce JSON Number value '1' to type Boolean",
            "locations" : [ { "line" : 5, "column" : 9 } ],
            "path" : [ "badBool" ]
          }
        ],
        "data" : {
          "badInt" : null,
          "badFloat" : null,
          "badBool" : null
        }
      }
    """

    val res = TestCirceScalarCoercionMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("filters compare coerced values") {
    import TestCirceScalarCoercionMapping._

    val cursor =
      circeCursor(
        Path.from(QueryType),
        Env.empty,
        json"""{ "bigInt": 3000000000, "intFromString": "42", "idFromInt": 23, "boolFromString": "true" }"""
      )

    def eql(field: String, value: Json): Predicate =
      Eql(UniquePath[Json](List(field)), Const(value))

    assertEquals(eql("bigInt", Json.fromLong(3000000000L))(cursor).toOption, Some(true))
    assertEquals(eql("intFromString", Json.fromInt(42))(cursor).toOption, Some(true))
    assertEquals(eql("intFromString", Json.fromString("42"))(cursor).toOption, Some(false))
    assertEquals(eql("idFromInt", Json.fromString("23"))(cursor).toOption, Some(true))
    assertEquals(eql("boolFromString", Json.True)(cursor).toOption, Some(true))
  }
}
