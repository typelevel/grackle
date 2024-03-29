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

package compiler

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import Query._
import Predicate._, Value._
import QueryCompiler._

object ItemData {
  case class Item(label: String, tags: List[String])

  val items =
    List(Item("A", List("A")), Item("AB", List("A", "B")), Item("BC", List("B", "C")), Item("C", List("C")))
}

object ItemMapping extends ValueMapping[IO] {
  import ItemData._

  val schema =
    schema"""
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

  val QueryType = schema.ref("Query")
  val ItemType = schema.ref("Item")

  val typeMappings =
    List(
      ValueObjectMapping[Unit](
        tpe = QueryType,
        fieldMappings =
          List(
            ValueField("itemByTag", _ => items),
            ValueField("itemByTagCount", _ => items),
            ValueField("itemByTagCountVA", _ => items),
            ValueField("itemByTagCountCA", _ => items)
          )
      ),
      ValueObjectMapping[Item](
        tpe = ItemType,
        fieldMappings =
          List(
            ValueField("label", _.label),
            ValueField("tags", _.tags),
            CursorField("tagCount", tagCount),
            ValueField("tagCountVA", _.tags.size, hidden = true),
            CursorField("tagCountCA", tagCount, hidden = true)
          )
      )
    )

  def tagCount(c: Cursor): Result[Int] =
    c.fieldAs[List[String]]("tags").map(_.size)

  override val selectElaborator = SelectElaborator {
    case (QueryType, "itemByTag", List(Binding("tag", IDValue(tag)))) =>
      Elab.transformChild(child => Filter(Contains(ItemType / "tags", Const(tag)), child))
    case (QueryType, "itemByTagCount", List(Binding("count", IntValue(count)))) =>
      Elab.transformChild(child => Filter(Eql(ItemType / "tagCount", Const(count)), child))
    case (QueryType, "itemByTagCountVA", List(Binding("count", IntValue(count)))) =>
      Elab.transformChild(child => Filter(Eql(ItemType / "tagCountVA", Const(count)), child))
    case (QueryType, "itemByTagCountCA", List(Binding("count", IntValue(count)))) =>
      Elab.transformChild(child => Filter(Eql(ItemType / "tagCountCA", Const(count)), child))
  }
}

final class PredicatesSuite extends CatsEffectSuite {
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

    val res = ItemMapping.compileAndRun(query)

    assertIO(res, expected)
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

    val res = ItemMapping.compileAndRun(query)

    assertIO(res, expected)
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

    val res = ItemMapping.compileAndRun(query)

    assertIO(res, expected)
  }
}
