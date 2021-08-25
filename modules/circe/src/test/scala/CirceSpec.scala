// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package circetests

import cats.tests.CatsSuite
import edu.gemini.grackle.syntax._

final class CirceSpec extends CatsSuite {
  test("scalars") {
    val query = """
      query {
        root {
          bool
          int
          float
          string
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
            "id": "bar"
          }
        }
      }
    """

    val res = TestCirceMapping.compileAndRun(query)
    //println(res)

    assert(res == expected)
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
    //println(res)

    assert(res == expected)
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
    //println(res)

    assert(res == expected)
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
    //println(res)

    assert(res == expected)
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
    //println(res)

    assert(res == expected)
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
    //println(res)

    assert(res == expected)
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
    //println(res)

    assert(res == expected)
  }
}
