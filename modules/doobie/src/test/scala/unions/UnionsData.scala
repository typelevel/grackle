// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package unions

import _root_.doobie.util.meta.Meta
import _root_.doobie.util.transactor.Transactor
import cats.effect.Sync
import edu.gemini.grackle._
import edu.gemini.grackle.doobie._
import edu.gemini.grackle.syntax._

trait UnionsMapping[F[_]] extends DoobieMapping[F] {

  object collections extends TableDef("collections") {
    val id = col("id", Meta[String])
    val itemType = col("item_type", Meta[String])
    val itemA = col("itema", Meta[String])
    val itemB = col("itemb", Meta[String])
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

  def itemTypeDiscriminator(c: Cursor): Result[Type] =
    for {
      it <- c.fieldAs[String]("itemType")
    } yield it match {
      case "ItemA" => ItemAType
      case "ItemB" => ItemBType
    }
}

object UnionsMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with UnionsMapping[F]

}
