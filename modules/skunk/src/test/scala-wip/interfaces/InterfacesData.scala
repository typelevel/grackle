// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package interfaces

import cats.effect.Sync
import cats.kernel.Eq
import skunk.Transactor
import skunk.util.meta.Meta
import io.circe.Encoder

import edu.gemini.grackle._, skunk._

trait InterfacesMapping[F[_]] extends SkunkMapping[F] {
  val schema =
    Schema(
      """
        type Query {
          entities: [Entity!]!
        }
        interface Entity {
          id: ID!
          entityType: EntityType!
          title: String!
          synopses: Synopses!
        }
        type Film implements Entity {
          id: ID!
          entityType: EntityType!
          title: String!
          synopses: Synopses!
          rating: String
        }
        type Series implements Entity {
          id: ID!
          entityType: EntityType!
          title: String!
          numberOfEpisodes: Int!
          episodes: [Episode!]!
        }
        type Episode {
          id: ID!
          title: String!
          synopses: Synopses!
        }
        type Synopses {
          short: String!
          long: String!
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

  import SkunkFieldMapping._

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SkunkRoot("entities"),
          )
      ),
      SkunkInterfaceMapping(
        tpe = EType,
        discriminator = entityTypeDiscriminator,
        fieldMappings =
          List(
            SkunkField("id", ColumnRef("entities", "id"), key = true),
            SkunkField("entityType", ColumnRef("entities", "entity_type"), discriminator = true),
            SkunkField("title", ColumnRef("entities", "title"))
          )
      ),
      ObjectMapping(
        tpe = FilmType,
        fieldMappings =
          List(
            SkunkField("rating", ColumnRef("entities", "film_rating")),
            SkunkObject("synopses", Subobject(Nil))
          )
      ),
      ObjectMapping(
        tpe = SeriesType,
        fieldMappings =
          List(
            SkunkField("numberOfEpisodes", ColumnRef("entities", "series_number_of_episodes")),
            SkunkObject("episodes", Subobject(
              List(Join(ColumnRef("entities", "id"), ColumnRef("episodes", "series_id")))
            ))
          )
      ),
      ObjectMapping(
        tpe = EpisodeType,
        fieldMappings =
          List(
            SkunkField("id", ColumnRef("episodes", "id"), key = true),
            SkunkField("title", ColumnRef("episodes", "title")),
            SkunkAttribute[String]("episodeId", ColumnRef("episodes", "series_id")),
            SkunkObject("synopses", Subobject(Nil))
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
                    SkunkField("short", ColumnRef("entities", "synopsis_short")),
                    SkunkField("long", ColumnRef("entities", "synopsis_long"))
                  )
              ),
            List("entities", "episodes", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SkunkField("short", ColumnRef("episodes", "synopsis_short")),
                    SkunkField("long", ColumnRef("episoes", "synopsis_long"))
                  )
              )
          )
      ),
      SkunkLeafMapping[EntityType](EntityTypeType)
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
  def mkMapping[F[_] : Sync](transactor: Transactor[F], monitor: SkunkMonitor[F]): InterfacesMapping[F] =
    new SkunkMapping(transactor, monitor) with InterfacesMapping[F]
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
