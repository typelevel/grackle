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
 * A non-null field beneath a nullable one must not remove rows whose nullable field is absent.
 *
 * Both joins are individually correct — the nullable field's is a LEFT JOIN, the non-null
 * field's an INNER JOIN — but flattening them into one chain lets the INNER JOIN eliminate the
 * rows the LEFT JOIN null-padded. The non-null-ness of `c` only constrains anything when a `B`
 * exists at all: per the GraphQL spec, completing a nullable field with a null result returns
 * null without executing its sub-selections.
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
}
