// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
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
import grackle.*
import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO
import io.circe.literal.*
import munit.CatsEffectSuite

trait SqlIssue606Suite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("no fragments: 3 elements") {
    val query = """
      query {
        instruction(id: 10) {
          sequence {
            id
            stepType
          }
        }
      }
    """

    val expected = json"""
      {
        "data": {
          "instruction": {
            "sequence": [
              {
                "id": 50,
                "stepType": "TEXT_ONLY_STEP"
              },
              {
                "id": 51,
                "stepType": "INSTRUCTION_CALL"
              },
              {
                "id": 52,
                "stepType": "INSTRUCTION_CALL"
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("fragment: two elements") {
    val query = """
      query {
        instruction(id: 10) {
          sequence {
            id
            stepType
            ... on TextualStep {
              id
              stepType
              description {
                id
                plain
                html
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data": {
          "instruction": {
            "sequence": [
              {
                "id": 51,
                "stepType": "INSTRUCTION_CALL"
              },
              {
                "id": 52,
                "stepType": "INSTRUCTION_CALL"
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
