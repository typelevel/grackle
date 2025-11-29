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

package compiler

import io.circe.literal._
import munit.CatsEffectSuite

final class AttributesSuite extends CatsEffectSuite {
  test("fields only") {
    val query = """
      query {
        itemByTag(tag: "A") {
          label
          tags
          tagCount
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "itemByTag" : [
            {
              "label" : "A",
              "tags" : [
                "A"
              ],
              "tagCount" : 1
            },
            {
              "label" : "AB",
              "tags" : [
                "A",
                "B"
              ],
              "tagCount" : 2
            }
          ]
        }
      }
    """

    val res = ItemMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("no value attributes") {
    val query = """
      query {
        itemByTag(tag: "A") {
          label
          tagCountVA
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "No field 'tagCountVA' for type Item"
          }
        ]
      }
    """

    val res = ItemMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("no cursor attributes") {
    val query = """
      query {
        itemByTag(tag: "A") {
          label
          tagCountCA
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "No field 'tagCountCA' for type Item"
          }
        ]
      }
    """

    val res = ItemMapping.compileAndRun(query)

    assertIO(res, expected)
  }
}
