// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import cats.effect.IO
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import cats.effect.unsafe.implicits.global

import edu.gemini.grackle._
import syntax._

import GraphQLResponseTests.assertWeaklyEqual

trait SqlFilterLimitOrderBySpec extends AnyFunSuite {
  def mapping: QueryExecutor[IO, Json]

  test("base query") {
    val query = """
      query {
        root {
          listA {
            id
            elemA
          }
          listB {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                },
                {
                  "id" : "b3",
                  "elemB" : 11
                }
              ]
            },
            {
              "listA" : [
                {
                  "id" : "a4",
                  "elemA" : "foo1"
                },
                {
                  "id" : "a5",
                  "elemA" : "bar1"
                },
                {
                  "id" : "a6",
                  "elemA" : "baz1"
                },
                {
                  "id" : "a7",
                  "elemA" : "quux1"
                }
              ],
              "listB" : [
                {
                  "id" : "b4",
                  "elemB" : 231
                },
                {
                  "id" : "b5",
                  "elemB" : 131
                },
                {
                  "id" : "b6",
                  "elemB" : 171
                },
                {
                  "id" : "b7",
                  "elemB" : 111
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("root filter") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          listA {
            id
            elemA
          }
          listB {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                },
                {
                  "id" : "b3",
                  "elemB" : 11
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("filter on one side") {
    val query = """
      query {
        root {
          listA(filter: { id: "a2" }) {
            id
            elemA
          }
          listB {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a2",
                  "elemA" : "baz"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                },
                {
                  "id" : "b3",
                  "elemB" : 11
                }
              ]
            },
            {
              "listA" : [
              ],
              "listB" : [
                {
                  "id" : "b4",
                  "elemB" : 231
                },
                {
                  "id" : "b5",
                  "elemB" : 131
                },
                {
                  "id" : "b6",
                  "elemB" : 171
                },
                {
                  "id" : "b7",
                  "elemB" : 111
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("filter on both sides") {
    val query = """
      query {
        root {
          listA(filter: { id: "a2" }) {
            id
            elemA
          }
          listB(filter: { id: "b1" }) {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a2",
                  "elemA" : "baz"
                }
              ],
              "listB" : [
                {
                  "id" : "b1",
                  "elemB" : 13
                }
              ]
            },
            {
              "listA" : [
              ],
              "listB" : [
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("nested filters") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          listA(filter: { id: "a2" }) {
            id
            elemA
          }
          listB {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a2",
                  "elemA" : "baz"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                },
                {
                  "id" : "b3",
                  "elemB" : 11
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("base alias") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          one: listA {
            id
            elemA
          }
          two: listA {
            id
            elemA
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "one" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ],
              "two" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("alias with filter on one side") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          one: listA(filter: { id: "a2" }) {
            id
            elemA
          }
          two: listA {
            id
            elemA
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "one" : [
                {
                  "id" : "a2",
                  "elemA" : "baz"
                }
              ],
              "two" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("alias with filter on both sides") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          one: listA(filter: { id: "a2" }) {
            id
            elemA
          }
          two: listA(filter: { id: "a3" }) {
            id
            elemA
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "one" : [
                {
                  "id" : "a2",
                  "elemA" : "baz"
                }
              ],
              "two" : [
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("root limit") {
    val query = """
      query {
        root(limit: 1) {
          listA {
            id
            elemA
          }
          listB {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                },
                {
                  "id" : "b3",
                  "elemB" : 11
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("limit on one side") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          listA(limit: 2) {
            id
            elemA
          }
          listB {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                },
                {
                  "id" : "b3",
                  "elemB" : 11
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("limit on both sides") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          listA(limit: 2) {
            id
            elemA
          }
          listB(limit: 3) {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("nested limit") {
    val query = """
      query {
        root(limit: 1) {
          listA(limit: 2) {
            id
            elemA
          }
          listB {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                },
                {
                  "id" : "b3",
                  "elemB" : 11
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("alias with limit on one side") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          one: listA(limit: 2) {
            id
            elemA
          }
          two: listA {
            id
            elemA
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "one" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                }
              ],
              "two" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("alias with limit on the other side") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          one: listA {
            id
            elemA
          }
          two: listA(limit: 2) {
            id
            elemA
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "one" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ],
              "two" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("alias with limit on both sides") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          one: listA(limit: 2) {
            id
            elemA
          }
          two: listA(limit: 3) {
            id
            elemA
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "one" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                }
              ],
              "two" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }

  test("order on one side") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          listA(order: ASC) {
            id
            elemA
          }
          listB {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                },
                {
                  "id" : "b3",
                  "elemB" : 11
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected, strictPaths = List(List("root", "listA")))
  }

  test("order on the other side") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          listA {
            id
            elemA
          }
          listB(order: DESC) {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b3",
                  "elemB" : 11
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected, strictPaths = List(List("root", "listB")))
  }

  test("order on both sides") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          listA(order: ASC) {
            id
            elemA
          }
          listB(order: DESC) {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b3",
                  "elemB" : 11
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected, strictPaths = List(List("root", "listA"), List("root", "listB")))
  }

  test("alias with order on one side") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          one: listA(order: DESC) {
            id
            elemA
          }
          two: listA {
            id
            elemA
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "one" : [
                {
                  "id" : "a3",
                  "elemA" : "quux"
                },
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                }
              ],
              "two" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected, strictPaths = List(List("root", "one")))
  }

  test("alias with order on both sides") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          one: listA(order: DESC) {
            id
            elemA
          }
          two: listA(order: ASC) {
            id
            elemA
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "one" : [
                {
                  "id" : "a3",
                  "elemA" : "quux"
                },
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                }
              ],
              "two" : [
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected, strictPaths = List(List("root", "one"), List("root", "two")))
  }

  test("order and limit on one side") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          listA(order: ASC, limit: 1) {
            id
            elemA
          }
          listB {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a1",
                  "elemA" : "bar"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                },
                {
                  "id" : "b1",
                  "elemB" : 13
                },
                {
                  "id" : "b2",
                  "elemB" : 17
                },
                {
                  "id" : "b3",
                  "elemB" : 11
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected, strictPaths = List(List("root", "listA")))
  }

  test("order and limit on the other side") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          listA {
            id
            elemA
          }
          listB(order: DESC, limit: 1) {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected, strictPaths = List(List("root", "listB")))
  }

  test("order and limit on both sides") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          listA(order: ASC, limit: 1) {
            id
            elemA
          }
          listB(order: DESC, limit: 1) {
            id
            elemB
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                {
                  "id" : "a1",
                  "elemA" : "bar"
                }
              ],
              "listB" : [
                {
                  "id" : "b0",
                  "elemB" : 23
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected, strictPaths = List(List("root", "listA"), List("root", "listB")))
  }

  test("alias with order and limit on one side") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          one: listA(order: ASC, limit: 1) {
            id
            elemA
          }
          two: listA {
            id
            elemA
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "one" : [
                {
                  "id" : "a1",
                  "elemA" : "bar"
                }
              ],
              "two" : [
                {
                  "id" : "a0",
                  "elemA" : "foo"
                },
                {
                  "id" : "a1",
                  "elemA" : "bar"
                },
                {
                  "id" : "a2",
                  "elemA" : "baz"
                },
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected, strictPaths = List(List("root", "one")))
  }

  test("alias with order and limit on both sides") {
    val query = """
      query {
        root(filter: { id: "r0" }) {
          one: listA(order: ASC, limit: 1) {
            id
            elemA
          }
          two: listA(order: DESC, limit: 1) {
            id
            elemA
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "one" : [
                {
                  "id" : "a1",
                  "elemA" : "bar"
                }
              ],
              "two" : [
                {
                  "id" : "a3",
                  "elemA" : "quux"
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected, strictPaths = List(List("root", "one"), List("root", "two")))
  }
}
