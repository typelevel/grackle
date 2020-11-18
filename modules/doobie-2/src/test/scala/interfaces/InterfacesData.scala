// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package interfaces

import _root_.doobie.util.meta.Meta
import _root_.doobie.util.transactor.Transactor
import cats.effect.Sync
import cats.kernel.Eq
import edu.gemini.grackle._
import edu.gemini.grackle.doobie._
import io.circe.Encoder

trait InterfacesMapping[F[_]] extends DoobieMapping[F] {
  val schema =
    Schema(
      """
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
    ).right.get

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
            SqlField("id", ColumnRef("entities", "id", Meta[String]), key = true),
            SqlField("entityType", ColumnRef("entities", "entity_type", Meta[EntityType]), discriminator = true),
            SqlField("title", ColumnRef("entities", "title", Meta[String]))
          )
      ),
      ObjectMapping(
        tpe = FilmType,
        fieldMappings =
          List(
            SqlField("rating", ColumnRef("entities", "film_rating", Meta[String])),
            SqlObject("synopses")
          )
      ),
      ObjectMapping(
        tpe = SeriesType,
        fieldMappings =
          List(
            SqlField("numberOfEpisodes", ColumnRef("entities", "series_number_of_episodes", Meta[Int])),
            SqlObject("episodes", Join(ColumnRef("entities", "id", Meta[String]), ColumnRef("episodes", "series_id", Meta[String]))),
          )
      ),
      ObjectMapping(
        tpe = EpisodeType,
        fieldMappings =
          List(
            SqlField("id", ColumnRef("episodes", "id", Meta[String]), key = true),
            SqlField("title", ColumnRef("episodes", "title", Meta[String])),
            SqlAttribute("episodeId", ColumnRef("episodes", "series_id", Meta[String])),
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
                    SqlField("short", ColumnRef("entities", "synopsis_short", Meta[String])),
                    SqlField("long", ColumnRef("entities", "synopsis_long", Meta[String]))
                  )
              ),
            List("entities", "episodes", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SqlField("short", ColumnRef("episodes", "synopsis_short", Meta[String])),
                    SqlField("long", ColumnRef("episoes", "synopsis_long", Meta[String]))
                  )
              )
          )
      ),
      SqlLeafMapping[EntityType](EntityTypeType, Meta[EntityType])
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

object InterfacesMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with InterfacesMapping[F]

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

  implicit val entityTypeMeta: Meta[EntityType] =
    Meta[Int].timap {
      case 1 => Film
      case 2 => Series
    } {
      case Film  => 1
      case Series => 2
    }

 }
