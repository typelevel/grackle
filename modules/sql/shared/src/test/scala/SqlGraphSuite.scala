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
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlGraphSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("root query") {
    val query = """
      query {
        node(id: 1) {
          id
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "node" : {
            "id" : 1
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("neighbour query (1)") {
    val query = """
      query {
        node(id: 1) {
          id
          neighbours {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "node" : {
            "id" : 1,
            "neighbours" : [
              {
                "id" : 2
              },
              {
                "id" : 3
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("neighbour query (2)") {
    val query = """
      query {
        node(id: 1) {
          id
          neighbours {
            id
            neighbours {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "node" : {
            "id" : 1,
            "neighbours" : [
              {
                "id" : 2,
                "neighbours" : [
                  {
                    "id" : 4
                  }
                ]
              },
              {
                "id" : 3,
                "neighbours" : [
                  {
                    "id" : 4
                  }
                ]
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
