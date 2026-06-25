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

package grackle.docs

import cats.effect.IO

import grackle._
import grackle.Predicate._
import grackle.Query._
import grackle.QueryCompiler._
import grackle.Value._
import grackle.syntax._

// #filter
object FilterMapping extends ValueMapping[IO] {

  case class Item(label: String, tags: List[String])

  val items =
    List(
      Item("A", List("A")),
      Item("AB", List("A", "B")),
      Item("BC", List("B", "C")),
      Item("C", List("C"))
    )

  val schema =
    schema"""
      type Query {
        itemsByTag(tag: ID!): [Item!]!
        itemsByTagCount(count: Int!): [Item!]!
      }
      type Item {
        label: String!
        tags: [String!]!
        tagCount: Int!
      }
    """

  val QueryType = schema.ref("Query")
  val ItemType  = schema.ref("Item")

  val typeMappings =
    List(
      ValueObjectMapping[Unit](
        tpe = QueryType,
        fieldMappings =
          List(
            ValueField("itemsByTag", _ => items),
            ValueField("itemsByTagCount", _ => items)
          )
      ),
      ValueObjectMapping[Item](
        tpe = ItemType,
        fieldMappings =
          List(
            ValueField("label", _.label),
            ValueField("tags", _.tags),
            // `tagCount` has no backing field; it is computed from the cursor.
            CursorField("tagCount", tagCount)
          )
      )
    )

  def tagCount(c: Cursor): Result[Int] =
    c.fieldAs[List[String]]("tags").map(_.size)

  override val selectElaborator =
    SelectElaborator {
      // `Contains` tests membership of a list-valued field.
      case (QueryType, "itemsByTag", List(Binding("tag", IDValue(tag)))) =>
        Elab.transformChild(child => Filter(Contains(ItemType / "tags", Const(tag)), child))
      // `Eql` compares a (here computed) field against a constant.
      case (QueryType, "itemsByTagCount", List(Binding("count", IntValue(count)))) =>
        Elab.transformChild(child => Filter(Eql(ItemType / "tagCount", Const(count)), child))
    }
}
// #filter
