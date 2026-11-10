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

/**
 * A non-null field beneath a nullable one, or beneath a list, must not remove rows whose parent
 * is absent or empty.
 *
 * The non-null-ness of such a field only constrains anything where its parent exists at all:
 * per the GraphQL spec, completing a nullable field with a null result returns null without
 * executing its sub-selections, and a `D` with no `E`s must still be returned with `es` as
 * `[]`.
 */
trait SqlNullableParentSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("a nullable field that is absent does not remove its row") {
    val query = """
      query {
        as {
          name
          b {
            name
            c {
              name
            }
          }
        }
      }
    """

    // `a-with-dangling-b` names a `B` which doesn't exist, so its row is reported with a null
    // `b` rather than as an error. That isn't what the spec asks for — a non-null field with no
    // row should raise an execution error propagated to the nearest nullable ancestor — but it
    // is a separate defect from the one under test here, and returning the row is already an
    // improvement on dropping it. Note that expecting `data` alone also expects no `errors`
    // entry, so this has to be revisited when that defect is addressed.
    val expected = json"""
      {
        "data" : {
          "as" : [
            {
              "name" : "a-with-good-b",
              "b" : {
                "name" : "b-with-c",
                "c" : {
                  "name" : "cat-1"
                }
              }
            },
            {
              "name" : "a-with-dangling-b",
              "b" : null
            },
            {
              "name" : "a-without-b",
              "b" : null
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("the same query stopping above the non-null field is unaffected") {
    val query = """
      query {
        as {
          name
          b {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "as" : [
            {
              "name" : "a-with-good-b",
              "b" : {
                "name" : "b-with-c"
              }
            },
            {
              "name" : "a-with-dangling-b",
              "b" : {
                "name" : "b-with-dangling-c"
              }
            },
            {
              "name" : "a-without-b",
              "b" : null
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("a list field that is empty does not remove its row") {
    val query = """
      query {
        ds {
          name
          es {
            name
            f {
              name
            }
          }
        }
      }
    """

    // `d-without-es` is the row at issue: it has no `E`s, so nothing beneath `es` is selected at
    // all, and the non-null-ness of `f` cannot bear on whether the `D` itself is returned.
    val expected = json"""
      {
        "data" : {
          "ds" : [
            {
              "name" : "d-with-es",
              "es" : [
                {
                  "name" : "e-with-f",
                  "f" : {
                    "name" : "fish-1"
                  }
                },
                {
                  "name" : "e-with-another-f",
                  "f" : {
                    "name" : "fish-2"
                  }
                }
              ]
            },
            {
              "name" : "d-without-es",
              "es" : []
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  // Reaching a second list through a non-null field nests the same shape twice, which on a
  // backend joining by `APPLY` presents the inner join to correlation a second time, once the
  // first has already rewritten it. `e-with-f` is the row at issue: its `otherD` is the `D`
  // with no `E`s, so only a query which descends that far has a row to lose.
  test("an empty list reached through a back reference does not remove its row") {
    val query = """
      query {
        ds {
          name
          es {
            name
            otherD {
              name
              es {
                name
                f {
                  name
                }
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "ds" : [
            {
              "name" : "d-with-es",
              "es" : [
                {
                  "name" : "e-with-f",
                  "otherD" : {
                    "name" : "d-without-es",
                    "es" : []
                  }
                },
                {
                  "name" : "e-with-another-f",
                  "otherD" : {
                    "name" : "d-with-es",
                    "es" : [
                      {
                        "name" : "e-with-f",
                        "f" : { "name" : "fish-1" }
                      },
                      {
                        "name" : "e-with-another-f",
                        "f" : { "name" : "fish-2" }
                      }
                    ]
                  }
                }
              ]
            },
            {
              "name" : "d-without-es",
              "es" : []
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("a filter through a nullable parent on a non-null field beneath it") {
    val query = """
      query {
        as(cName: "cat-1") {
          name
          b {
            name
            c {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "as" : [
            {
              "name" : "a-with-good-b",
              "b" : {
                "name" : "b-with-c",
                "c" : {
                  "name" : "cat-1"
                }
              }
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("a filter through a list on a non-null field beneath it") {
    val query = """
      query {
        ds(fName: "fish-2") {
          name
          es {
            name
            f {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "ds" : [
            {
              "name" : "d-with-es",
              "es" : [
                {
                  "name" : "e-with-f",
                  "f" : {
                    "name" : "fish-1"
                  }
                },
                {
                  "name" : "e-with-another-f",
                  "f" : {
                    "name" : "fish-2"
                  }
                }
              ]
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("an order through a nullable parent on a non-null field beneath it") {
    // `a-without-b` and `a-with-dangling-b` both order as null, so only the head is fixed.
    val query = """
      query {
        as(order: "deep") {
          name
        }
      }
    """

    mapping.compileAndRun(query).map { res =>
      val names = res.hcursor.downField("data").downField("as").as[List[Map[String, String]]]
      assertEquals(
        names.map(_.map(_("name"))).map(ns => (ns.size, ns.head)),
        Right((3, "a-with-good-b")))
    }
  }

  test("a filter through a nullable parent combined with an order on the parent itself") {
    // The filter nests `b` in a subquery, the order joins it directly. Read the order column
    // from the table, not the subquery.
    val query = """
      query {
        as(cName: "cat-1", order: "mid") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "as" : [
            {
              "name" : "a-with-good-b"
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("a filter and limit on a list through a non-null field beneath it") {
    // The filter's join must be in the FROM clause of the windowed select for the limit.
    val query = """
      query {
        ds {
          name
          es(fName: "fish-1", limit: 1) {
            name
            otherD {
              es {
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
          "ds" : [
            {
              "name" : "d-without-es",
              "es" : [
              ]
            },
            {
              "name" : "d-with-es",
              "es" : [
                {
                  "name" : "e-with-f",
                  "otherD" : {
                    "es" : [
                    ]
                  }
                }
              ]
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("an order through a nullable parent with a limit") {
    // Without a parent constraint, the limit is applied in a DISTINCT select over the keys.
    // That select must order by `b.name`, not by the `name` of the base table.
    val query = """
      query {
        as(order: "mid", limit: 1) {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "as" : [
            {
              "name" : "a-with-good-b"
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("an order through a nullable parent on a non-null field beneath it with a limit") {
    val query = """
      query {
        as(order: "deep", limit: 1) {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "as" : [
            {
              "name" : "a-with-good-b"
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("a filter and limit through a list on a non-null field beneath it") {
    val query = """
      query {
        ds(fName: "fish-2", limit: 1) {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "ds" : [
            {
              "name" : "d-with-es"
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("a filter through a list on a limited list beneath a list") {
    // The filter is not one-to-one, so the windowed select for the limit is built over the
    // keys alone, with the filter's joins only.
    val query = """
      query {
        ds {
          name
          es(otherFName: "fish-1", limit: 1) {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "ds" : [
            {
              "name" : "d-without-es",
              "es" : [
              ]
            },
            {
              "name" : "d-with-es",
              "es" : [
                {
                  "name" : "e-with-another-f"
                }
              ]
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("a filter through a list and an order on a limited list beneath a list") {
    val query = """
      query {
        ds {
          name
          es(otherFName: "fish-1", order: "f", limit: 1) {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "ds" : [
            {
              "name" : "d-without-es",
              "es" : [
              ]
            },
            {
              "name" : "d-with-es",
              "es" : [
                {
                  "name" : "e-with-another-f"
                }
              ]
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }
}
