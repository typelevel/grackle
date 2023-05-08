// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.effect.unsafe.implicits.global
import cats.tests.CatsSuite
import edu.gemini.grackle.syntax._

final class AttributesSpec extends CatsSuite {
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

    val res = ItemMapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assert(res == expected)
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
            "message" : "Unknown field 'tagCountVA' in select"
          }
        ]
      }
    """

    val res = ItemMapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assert(res == expected)
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
            "message" : "Unknown field 'tagCountCA' in select"
          }
        ]
      }
    """

    val res = ItemMapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assert(res == expected)
  }
}
