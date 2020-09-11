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

object ItemData {
  case class Item(label: String, tags: List[String])

  val items =
    List(Item("A", List("A")), Item("AB", List("A", "B")), Item("BC", List("B", "C")), Item("C", List("C")))
}

object ItemMapping extends ValueMapping[Id] {
  import ItemData._

  val schema =
    Schema(
      """
        type Query {
          itemByTag(tag: ID!): [Item!]!
          itemByTagCount(count: Int!): [Item!]!
          itemByTagCountVA(count: Int!): [Item!]!
          itemByTagCountCA(count: Int!): [Item!]!
        }
        type Item {
          label: String!
          tags: [String!]!
          tagCount: Int!
        }
      """
    ).right.get

  val QueryType = schema.ref("Query")
  val ItemType = schema.ref("Item")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            ValueRoot("itemByTag", items),
            ValueRoot("itemByTagCount", items),
            ValueRoot("itemByTagCountVA", items),
            ValueRoot("itemByTagCountCA", items)
          )
      ),
      ValueObjectMapping[Item](
        tpe = ItemType,
        fieldMappings =
          List(
            ValueField("label", _.label),
            ValueField("tags", _.tags),
            CursorField("tagCount", tagCount),
            ValueAttribute("tagCountVA", _.tags.size),
            CursorAttribute("tagCountCA", tagCount)
          )
      )
    )

  def tagCount(c: Cursor): Result[Int] =
    c.fieldAs[List[String]]("tags").map(_.size)

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("itemByTag", List(Binding("tag", IDValue(tag))), child) =>
        Select("itemByTag", Nil, Filter(Contains(CollectFieldPath(List("tags")), Const(tag)), child)).rightIor
      case Select("itemByTagCount", List(Binding("count", IntValue(count))), child) =>
        Select("itemByTagCount", Nil, Filter(Eql(FieldPath(List("tagCount")), Const(count)), child)).rightIor
      case Select("itemByTagCountVA", List(Binding("count", IntValue(count))), child) =>
        Select("itemByTagCountVA", Nil, Filter(Eql(AttrPath(List("tagCountVA")), Const(count)), child)).rightIor
      case Select("itemByTagCountCA", List(Binding("count", IntValue(count))), child) =>
        Select("itemByTagCountCA", Nil, Filter(Eql(AttrPath(List("tagCountCA")), Const(count)), child)).rightIor
    }
  ))
}

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

    val compiledQuery = ItemMapping.compiler.compile(query).right.get
    val res = ItemMapping.interpreter.run(compiledQuery, ItemMapping.QueryType)
    //println(res)

    assert(res == expected)
  }

  test("computed field") {
    val query = """
      query {
        itemByTagCount(count: 2) {
          label
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "itemByTagCount" : [
            {
              "label" : "AB"
            },
            {
              "label" : "BC"
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

  test("attributes") {
    val query = """
      query {
        itemByTagCountVA(count: 2) {
          label
        }
        itemByTagCountCA(count: 2) {
          label
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "itemByTagCountVA" : [
            {
              "label" : "AB"
            },
            {
              "label" : "BC"
            }
          ],
          "itemByTagCountCA" : [
            {
              "label" : "AB"
            },
            {
              "label" : "BC"
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
}
