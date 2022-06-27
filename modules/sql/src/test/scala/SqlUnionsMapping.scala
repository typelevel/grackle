// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import Path._, Predicate._

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
            SqlRoot("collection")
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

    def narrowPredicate(subtpe: Type): Option[Predicate] = {
      def mkPredicate(tpe: String): Option[Predicate] =
        Some(Eql(UniquePath(List("itemType")), Const(tpe)))

      subtpe match {
        case ItemAType => mkPredicate("ItemA")
        case ItemBType => mkPredicate("ItemB")
        case _ => None
      }
    }
  }
}
