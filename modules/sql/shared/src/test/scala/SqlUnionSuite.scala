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

trait SqlUnionSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("simple union query") {
    val query = """
      query {
        collection {
          ... on ItemA {
            itema
          }
          ... on ItemB {
            itemb
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "collection" : [
            {
              "itema" : "A"
            },
            {
              "itemb" : "B"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("union query with introspection") {
    val query = """
      query {
        collection {
          ... on ItemA {
            __typename
            itema
          }
          ... on ItemB {
            __typename
            itemb
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "collection" : [
            {
              "__typename" : "ItemA",
              "itema" : "A"
            },
            {
              "__typename" : "ItemB",
              "itemb" : "B"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("unrequested members of union returns empty response") {
    val query = """
      query {
        collection {
          ... on ItemA {
            itema
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "collection" : [
            {
              "itema" : "A"
            },
            {
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("union query with only introspection") {
    val query = """
      query {
        collection {
          ... on ItemA {
            __typename
          }
          ... on ItemB {
            __typename
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "collection" : [
            {
              "__typename" : "ItemA"
            },
            {
              "__typename" : "ItemB"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
