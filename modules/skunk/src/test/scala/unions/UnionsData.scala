// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package unions

import cats.effect.Sync
import _root_.skunk.codec.all._
import edu.gemini.grackle._, skunk._
import cats.effect.Resource
import _root_.skunk.Session

trait UnionsMapping[F[_]] extends SkunkMapping[F] {
  val schema =
    Schema(
      """
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
    ).right.get

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
            SqlAttribute("id", ColumnRef("collections", "id", text), key = true),
            SqlAttribute("itemType", ColumnRef("collections", "item_type", text), discriminator = true)
          )
      ),
      ObjectMapping(
        tpe = ItemAType,
        fieldMappings =
          List(
            SqlAttribute("id", ColumnRef("collections", "id", text), key = true),
            SqlField("itema", ColumnRef("collections", "itema", text))
          )
      ),
      ObjectMapping(
        tpe = ItemBType,
        fieldMappings =
          List(
            SqlAttribute("id", ColumnRef("collections", "id", text), key = true),
            SqlField("itemb", ColumnRef("collections", "itemb", text))
          )
      )
    )

  def itemTypeDiscriminator(c: Cursor): Result[Type] =
    for {
      it <- c.attributeAs[String]("itemType")
    } yield it match {
      case "ItemA" => ItemAType
      case "ItemB" => ItemBType
    }
}

object UnionsMapping extends SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with UnionsMapping[F]

}
