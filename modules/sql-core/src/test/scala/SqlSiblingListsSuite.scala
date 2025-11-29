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

trait SqlSiblingListsSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("base query") {
    val query = """
      query {
        a(id: "id_a_1") {
          bs {
            cs { nameC }
            ds { nameD }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "a" : {
            "bs" : [
              {
                "cs" : [
                  {
                    "nameC" : "name_c"
                  }
                ],
                "ds" : [
                  {
                    "nameD" : "name_d"
                  }
                ]
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)
    //println(res)

    assertWeaklyEqualIO(res, expected)
  }
}
