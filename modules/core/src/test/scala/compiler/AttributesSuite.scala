// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
