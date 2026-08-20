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

// Fixture rows (see testdata/pg/union-order.sql and testdata/mssql/qualified-union-order.sql) are
// seeded out of alphabetical order on purpose: id 1 "Charlie", id 2 "Alpha", id 3 "Bravo", id 4
// "Delta". If ordering silently doesn't apply, the top 2 by insertion order would be
// Charlie/Alpha, not the correct Alpha/Bravo - the assertion can't pass by coincidence.
trait SqlUnionOrderSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("union branch ordering with limit") {
    val query = """
      query {
        entities(order: ASC, limit: 2) {
          ... on ItemA { name }
          ... on ItemB { name }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            { "name" : "Alpha" },
            { "name" : "Bravo" }
          ]
        }
      }
    """

    assertWeaklyEqualIO(mapping.compileAndRun(query), expected)
  }
}
