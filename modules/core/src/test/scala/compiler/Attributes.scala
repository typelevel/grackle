// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import edu.gemini.grackle.QueryInterpreter.mkInvalidResponse

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

    val compiledQuery = ItemMapping.compiler.compile(query).right.get
    val res = ItemMapping.interpreter.run(compiledQuery, ItemMapping.QueryType)
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

    val res = mkInvalidResponse(ItemMapping.compiler.compile(query))
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

    val res = mkInvalidResponse(ItemMapping.compiler.compile(query))
    //println(res)

    assert(res == expected)
  }
}
