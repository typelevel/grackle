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

// Wired up for doobie-pg and skunk, which share the testdata/pg fixtures; the fix under test
// lives in sql-core so those two backends suffice to pin it. Oracle is omitted because its
// schemas are users (a qualified-name fixture needs dedicated user setup), and MSSQL because
// its fixture init would need equivalent schema plumbing - both can adopt this suite later.
trait SqlQualifiedNamesSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("recursive query against schema-qualified table names (#342)") {
    val query = """
      query {
        country(code: "CAN") {
          name
          cities {
            name
            country {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "Canada",
            "cities" : [
              {
                "name" : "Toronto",
                "country" : {
                  "name" : "Canada"
                }
              },
              {
                "name" : "Ottawa",
                "country" : {
                  "name" : "Canada"
                }
              }
            ]
          }
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  // Top-level limit over a list with a child join forces the compiler to synthesize named
  // subqueries (via syntheticName), a second path on which a schema-qualified table name
  // must not leak into alias position.
  test("top-level limit over schema-qualified tables (#342)") {
    val query = """
      query {
        countries(limit: 2) {
          name
          cities {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : [
            {
              "name" : "Canada",
              "cities" : [
                { "name" : "Toronto" },
                { "name" : "Ottawa" }
              ]
            },
            {
              "name" : "Germany",
              "cities" : [
                { "name" : "Berlin" }
              ]
            }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  // A limit nested below the root exercises the window-function machinery and its
  // synthesized subquery names.
  test("nested limit over schema-qualified tables (#342)") {
    val query = """
      query {
        country(code: "CAN") {
          name
          cities(limit: 1) {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "Canada",
            "cities" : [
              { "name" : "Ottawa" }
            ]
          }
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  // An associative child reached through a single mergeable join takes the DerivedTableRef
  // "_assoc" path, whose alias is derived from the raw table name - a further place a
  // schema-qualified name must not leak into alias position.
  test("associative field over schema-qualified tables (#342)") {
    val query = """
      query {
        country(code: "CAN") {
          name
          languages {
            language
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "Canada",
            "languages" : [
              { "language" : "English" },
              { "language" : "French" }
            ]
          }
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  // A limit on a node with two sibling list children compiles to an SqlUnion whose branches are
  // then wrapped in a subquery by SqlUnion.addFilterOrderByOffsetLimit. That subquery is named
  // from the parent table, so for a schema-qualified table the name has to be folded - but
  // folding it also changes the TableExpr's identity, since TableExprs are compared by name.
  // Columns still bound to the unfolded TableRef are then rendered qualified by the raw dotted
  // name while the FROM entry carries the folded one, e.g.
  //
  //   FROM ( ... UNION ALL ... ) AS qualified_city
  //   WHERE (qualified.city.<col> IS NOT NULL)
  //
  // For an unqualified table the folded and unfolded names coincide, so the divergence is
  // invisible; it only surfaces here. Note the limit must be nested - the same union under a
  // top-level limit renders correctly.
  test("nested limit over a union of schema-qualified tables (#342)") {
    val query = """
      query {
        country(code: "CAN") {
          cities(limit: 1) {
            name
            languages {
              language
            }
            twins {
              motto
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "cities" : [
              {
                "name" : "Ottawa",
                "languages" : [
                  { "language" : "English" },
                  { "language" : "French" }
                ],
                "twins" : [
                  { "motto" : "A mari usque ad mare" }
                ]
              }
            ]
          }
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  // qualified_country is a real table whose name equals qualified.country with its qualifier
  // folded by an underscore. Recursing through qualified.country while joining
  // qualified_country in the same statement pins that folded synthesized identifiers and
  // identically-named real tables coexist (the render-time alias state uniquifies any
  // second occurrence of an already-seen name).
  test("folded qualified name coexists with an identically named table (#342)") {
    val query = """
      query {
        country(code: "CAN") {
          name
          twin {
            motto
          }
          cities {
            name
            country {
              name
              twin {
                motto
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "Canada",
            "twin" : {
              "motto" : "A mari usque ad mare"
            },
            "cities" : [
              {
                "name" : "Toronto",
                "country" : {
                  "name" : "Canada",
                  "twin" : {
                    "motto" : "A mari usque ad mare"
                  }
                }
              },
              {
                "name" : "Ottawa",
                "country" : {
                  "name" : "Canada",
                  "twin" : {
                    "motto" : "A mari usque ad mare"
                  }
                }
              }
            ]
          }
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("offset/limit paging over schema-qualified table names (#342)") {
    val query = """
      query {
        paged(offset: 0, limit: 2) {
          items {
            code
            name
            cities(offset: 0, limit: 2) {
              items {
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
          "paged" : {
            "items" : [
              {
                "code" : "CAN",
                "name" : "Canada",
                "cities" : {
                  "items" : [
                    {
                      "name" : "Ottawa"
                    },
                    {
                      "name" : "Toronto"
                    }
                  ]
                }
              },
              {
                "code" : "DEU",
                "name" : "Germany",
                "cities" : {
                  "items" : [
                    {
                      "name" : "Berlin"
                    }
                  ]
                }
              }
            ]
          }
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }

  test("paging over a qualified table reached at two result paths (#342)") {
    val query = """
      query {
        paged(offset: 0, limit: 2) {
          total
          items {
            code
            name
            twin {
              motto
              cities(offset: 0, limit: 1) {
                items {
                  name
                }
              }
            }
            cities(offset: 0, limit: 2) {
              items {
                name
                country {
                  code
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
          "paged" : {
            "total" : 2,
            "items" : [
              {
                "code" : "CAN",
                "name" : "Canada",
                "twin" : {
                  "motto" : "A mari usque ad mare",
                  "cities" : {
                    "items" : [
                      {
                        "name" : "Ottawa"
                      }
                    ]
                  }
                },
                "cities" : {
                  "items" : [
                    {
                      "name" : "Ottawa",
                      "country" : {
                        "code" : "CAN",
                        "name" : "Canada"
                      }
                    },
                    {
                      "name" : "Toronto",
                      "country" : {
                        "code" : "CAN",
                        "name" : "Canada"
                      }
                    }
                  ]
                }
              },
              {
                "code" : "DEU",
                "name" : "Germany",
                "twin" : {
                  "motto" : "Einigkeit und Recht und Freiheit",
                  "cities" : {
                    "items" : [
                      {
                        "name" : "Berlin"
                      }
                    ]
                  }
                },
                "cities" : {
                  "items" : [
                    {
                      "name" : "Berlin",
                      "country" : {
                        "code" : "DEU",
                        "name" : "Germany"
                      }
                    }
                  ]
                }
              }
            ]
          }
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }
}
