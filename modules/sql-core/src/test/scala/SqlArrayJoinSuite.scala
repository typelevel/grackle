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
import grackle._
import io.circe.literal._
import munit.CatsEffectSuite

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlArrayJoinSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

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
                  "elemA" : ["foo1","foo2"]
                },
                {
                  "id" : "a1",
                  "elemA" : ["bar1","bar2"]
                },
                {
                  "id" : "a2",
                  "elemA" : ["baz1","baz2"]
                },
                {
                  "id" : "a3",
                  "elemA" : ["quux1","quux2"]
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
                  "elemA" : ["foo11","foo22"]
                },
                {
                  "id" : "a5",
                  "elemA" : ["bar11","bar22"]
                },
                {
                  "id" : "a6",
                  "elemA" : ["baz11","baz22"]
                },
                {
                  "id" : "a7",
                  "elemA" : ["quux11","quux22"]
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
