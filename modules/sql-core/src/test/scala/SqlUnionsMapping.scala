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

import grackle._
import grackle.syntax._
import Predicate._

trait SqlUnionsMapping[F[_]] extends SqlTestMapping[F] {

  object collections extends TableDef("collections") {
    val id = col("id", text)
    val itemType = col("item_type", text)
    val itemA = col("itema", text)
    val itemB = col("itemb", text)
  }

  val schema =
    schema"""
      type Query {
        collection: [Item!]!
      }
      type ItemA {
        itema: String!
      }
      type ItemB {
        itemb: String!
      }
      union Item = ItemA | ItemB
    """

  val QueryType = schema.ref("Query")
  val ItemAType = schema.ref("ItemA")
  val ItemBType = schema.ref("ItemB")
  val ItemType = schema.ref("Item")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlObject("collection")
          )
      ),
      SqlUnionMapping(
        tpe = ItemType,
        discriminator = itemTypeDiscriminator,
        fieldMappings =
          List(
            SqlField("id", collections.id, key = true, hidden = true),
            SqlField("itemType", collections.itemType, discriminator = true, hidden = true)
          )
      ),
      ObjectMapping(
        tpe = ItemAType,
        fieldMappings =
          List(
            SqlField("id", collections.id, key = true, hidden = true),
            SqlField("itema", collections.itemA)
          )
      ),
      ObjectMapping(
        tpe = ItemBType,
        fieldMappings =
          List(
            SqlField("id", collections.id, key = true, hidden = true),
            SqlField("itemb", collections.itemB)
          )
      )
    )

  object itemTypeDiscriminator extends SqlDiscriminator {
    def discriminate(c: Cursor): Result[Type] =
      for {
        it <- c.fieldAs[String]("itemType")
      } yield it match {
        case "ItemA" => ItemAType
        case "ItemB" => ItemBType
      }

    def narrowPredicate(subtpe: Type): Result[Predicate] = {
      def mkPredicate(tpe: String): Result[Predicate] =
        Eql(ItemType / "itemType", Const(tpe)).success

      subtpe match {
        case ItemAType => mkPredicate("ItemA")
        case ItemBType => mkPredicate("ItemB")
        case _ => Result.internalError(s"Invalid discriminator: $subtpe")
      }
    }
  }
}
