// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package unions

import _root_.doobie.util.meta.Meta
import _root_.doobie.util.transactor.Transactor
import cats.effect.Sync
import edu.gemini.grackle._
import edu.gemini.grackle.doobie._

trait UnionsMapping[F[_]] extends DoobieMapping[F] {
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
            SqlAttribute("id", ColumnRef("collections", "id", Meta[String]), key = true),
            SqlAttribute("itemType", ColumnRef("collections", "item_type", Meta[String]), discriminator = true)
          )
      ),
      ObjectMapping(
        tpe = ItemAType,
        fieldMappings =
          List(
            SqlAttribute("id", ColumnRef("collections", "id", Meta[String]), key = true),
            SqlField("itema", ColumnRef("collections", "itema", Meta[String]))
          )
      ),
      ObjectMapping(
        tpe = ItemBType,
        fieldMappings =
          List(
            SqlAttribute("id", ColumnRef("collections", "id", Meta[String]), key = true),
            SqlField("itemb", ColumnRef("collections", "itemb", Meta[String]))
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

object UnionsMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with UnionsMapping[F]

}
