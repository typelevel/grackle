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

trait SqlInterfacesSuite2 extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("when discriminator fails the fragments should be ignored") {
    val query = """
      query {
        entities {
          id
          entityType
          title
          ... on Film {
            rating
          }
          ... on Series {
            numberOfEpisodes
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "1",
              "entityType" : "FILM",
              "title" : "Film 1"
            },
            {
              "id" : "2",
              "entityType" : "FILM",
              "title" : "Film 2"
            },
            {
              "id" : "3",
              "entityType" : "FILM",
              "title" : "Film 3"
            },
            {
              "id" : "4",
              "entityType" : "SERIES",
              "title" : "Series 1"
            },
            {
              "id" : "5",
              "entityType" : "SERIES",
              "title" : "Series 2"
            },
            {
              "id" : "6",
              "entityType" : "SERIES",
              "title" : "Series 3"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
