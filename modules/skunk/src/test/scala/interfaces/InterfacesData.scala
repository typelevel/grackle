// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package interfaces

import cats.effect.Sync
import cats.kernel.Eq
import cats.syntax.all._
import io.circe.Encoder
import _root_.skunk.codec.all._
import edu.gemini.grackle._, skunk._, syntax._
import _root_.skunk.Codec
import cats.effect.Resource
import _root_.skunk.Session

trait InterfacesMapping[F[_]] extends SkunkMapping[F] {
  val schema =
    schema"""
      type Query {
        entities: [Entity!]!
      }
      interface Entity {
        id: ID!
        entityType: EntityType!
        title: String
        synopses: Synopses
      }
      type Film implements Entity {
        id: ID!
        entityType: EntityType!
        title: String
        synopses: Synopses
        rating: String
      }
      type Series implements Entity {
        id: ID!
        entityType: EntityType!
        title: String
        numberOfEpisodes: Int
        episodes: [Episode!]!
      }
      type Episode {
        id: ID!
        title: String
        synopses: Synopses
      }
      type Synopses {
        short: String
        long: String
      }
      enum EntityType {
        FILM
        SERIES
      }
    """

  val QueryType = schema.ref("Query")
  val EType = schema.ref("Entity")
  val EntityTypeType = schema.ref("EntityType")
  val FilmType = schema.ref("Film")
  val SeriesType = schema.ref("Series")
  val EpisodeType = schema.ref("Episode")
  val SynopsesType = schema.ref("Synopses")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlRoot("entities"),
          )
      ),
      SqlInterfaceMapping(
        tpe = EType,
        discriminator = entityTypeDiscriminator,
        fieldMappings =
          List(
            SqlField("id", ColumnRef("entities", "id", text), key = true),
            SqlField("entityType", ColumnRef("entities", "entity_type", EntityType.codec), discriminator = true),
            SqlField("title", ColumnRef("entities", "title", text.opt))
          )
      ),
      ObjectMapping(
        tpe = FilmType,
        fieldMappings =
          List(
            SqlField("rating", ColumnRef("entities", "film_rating", text.opt)),
            SqlObject("synopses")
          )
      ),
      ObjectMapping(
        tpe = SeriesType,
        fieldMappings =
          List(
            SqlField("numberOfEpisodes", ColumnRef("entities", "series_number_of_episodes", int4.opt)),
            SqlObject("episodes", Join(ColumnRef("entities", "id", text), ColumnRef("episodes", "series_id", text))),
          )
      ),
      ObjectMapping(
        tpe = EpisodeType,
        fieldMappings =
          List(
            SqlField("id", ColumnRef("episodes", "id", text), key = true),
            SqlField("title", ColumnRef("episodes", "title", text.opt)),
            SqlField("episodeId", ColumnRef("episodes", "series_id", text), hidden = true),
            SqlObject("synopses"),
          )
      ),
      PrefixedMapping(
        tpe = SynopsesType,
        mappings =
          List(
            List("entities", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SqlField("short", ColumnRef("entities", "synopsis_short", text.opt)),
                    SqlField("long", ColumnRef("entities", "synopsis_long", text.opt))
                  )
              ),
            List("entities", "episodes", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SqlField("short", ColumnRef("episodes", "synopsis_short", text.opt)),
                    SqlField("long", ColumnRef("episoes", "synopsis_long", text.opt))
                  )
              )
          )
      ),
      LeafMapping[EntityType](EntityTypeType)
    )

  def entityTypeDiscriminator(c: Cursor): Result[Type] = {
    for {
      et <- c.fieldAs[EntityType]("entityType")
    } yield et match {
      case EntityType.Film => FilmType
      case EntityType.Series => SeriesType
    }
  }
}

object InterfacesMapping extends SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with InterfacesMapping[F]

}


sealed trait EntityType extends Product with Serializable
object EntityType {
  case object Film extends EntityType
  case object Series extends EntityType

  implicit val entityTypeEq: Eq[EntityType] = Eq.fromUniversalEquals[EntityType]

  def fromString(s: String): Option[EntityType] =
    s.trim.toUpperCase match {
      case "FILM"  => Some(Film)
      case "SERIES" => Some(Series)
      case _ => None
    }

  implicit val entityTypeEncoder: Encoder[EntityType] =
    Encoder[String].contramap(_ match {
      case Film => "FILM"
      case Series => "SERIES"
    })

  implicit val codec: Codec[EntityType] =
    Codec.simple(
      {
        case Film   => "1"
        case Series => "2"
      }, {
        case "1" => Film.asRight
        case "2" => Series.asRight
        case n   => s"No such entity type: $n".asLeft
      },
      _root_.skunk.data.Type.int4
    )

 }
