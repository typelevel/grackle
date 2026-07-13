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
}
