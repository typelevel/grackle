// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.Id
import cats.implicits._
import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import edu.gemini.grackle._
import Query._, Predicate._, Value._
import QueryCompiler._

import ItemData._
import ItemSchema._

object ItemSchema {
  val schema =
    Schema(
      """
        type Query {
          itemByTag(tag: ID!): [Item!]!
        }
        type Item {
          label: String!
          tags: [String!]!
        }
      """
    ).right.get
}

object ItemData {

  val QueryType = schema.ref("Query")
  val ItemType = schema.ref("Item")

  case class Item(label: String, tags: List[String])

  val items =
    List(Item("A", List("A")), Item("AB", List("A", "B")), Item("BC", List("B", "C")), Item("C", List("C")))
}

object ItemQueryCompiler extends QueryCompiler(schema) {
  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("itemByTag", List(Binding("tag", IDValue(tag))), child) =>
        Select("itemByTag", Nil, Filter(Contains(FieldPath(List("tags")), Const(tag)), child)).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

object ItemQueryInterpreter extends DataTypeQueryInterpreter[Id](
  {
    case "itemByTag" => (ListType(ItemType), items)
  },
  {
    case (c: Item, "label") => c.label
    case (c: Item, "tags")  => c.tags
  }
)

final class PredicatesSpec extends CatsSuite {
  test("simple query") {
    val query = """
      query {
        a: itemByTag(tag: "A") { label }
        b: itemByTag(tag: "B") { label }
        c: itemByTag(tag: "C") { label }
      }
    """

    val expected = json"""
      {
        "data" : {
          "a" : [
            {
              "label" : "A"
            },
            {
              "label" : "AB"
            }
          ],
          "b" : [
            {
              "label" : "AB"
            },
            {
              "label" : "BC"
            }
          ],
          "c" : [
            {
              "label" : "BC"
            },
            {
              "label" : "C"
            }
          ]
        }
      }
    """

    val compiledQuery = ItemQueryCompiler.compile(query).right.get
    val res = ItemQueryInterpreter.run(compiledQuery, ItemData.QueryType)
    //println(res)

    assert(res == expected)
  }
}
