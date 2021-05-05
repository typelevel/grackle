// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package interfaces

import cats.effect.Sync
import cats.kernel.Eq
import _root_.doobie.util.meta.Meta
import _root_.doobie.util.transactor.Transactor
import io.circe.Encoder

import edu.gemini.grackle._
import edu.gemini.grackle.doobie._
import edu.gemini.grackle.syntax._

trait InterfacesMapping[F[_]] extends DoobieMapping[F] {

  object entities extends TableDef("entities") {
    val id                     = col("id", Meta[String])
    val entityType             = col("entity_type", Meta[EntityType])
    val title                  = col("title", Meta[String], true)
    val filmRating             = col("film_rating", Meta[String], true)
    val seriesNumberOfEpisodes = col("series_number_of_episodes", Meta[Int], true)
    val synopsisShort          = col("synopsis_short", Meta[String], true)
    val synopsisLong           = col("synopsis_long", Meta[String], true)
  }

  object episodes extends TableDef("episodes") {
    val id                     = col("id", Meta[String])
    val title                  = col("title", Meta[String], true)
    val seriesId               = col("series_id", Meta[String])
    val synopsisShort          = col("synopsis_short", Meta[String], true)
    val synopsisLong           = col("synopsis_long", Meta[String], true)
  }

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
            SqlRoot("entities")
          )
      ),
      SqlInterfaceMapping(
        tpe = EType,
        discriminator = entityTypeDiscriminator,
        fieldMappings =
          List(
            SqlField("id", entities.id, key = true),
            SqlField("entityType", entities.entityType, discriminator = true),
            SqlField("title", entities.title)
          )
      ),
      ObjectMapping(
        tpe = FilmType,
        fieldMappings =
          List(
            SqlField("rating", entities.filmRating),
            SqlObject("synopses")
          )
      ),
      ObjectMapping(
        tpe = SeriesType,
        fieldMappings =
          List(
            SqlField("numberOfEpisodes", entities.seriesNumberOfEpisodes),
            SqlObject("episodes", Join(entities.id, episodes.seriesId)),
          )
      ),
      ObjectMapping(
        tpe = EpisodeType,
        fieldMappings =
          List(
            SqlField("id", episodes.id, key = true),
            SqlField("title", episodes.title),
            SqlField("episodeId", episodes.seriesId, hidden = true),
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
                    SqlField("short", entities.synopsisShort),
                    SqlField("long", entities.synopsisLong)
                  )
              ),
            List("entities", "episodes", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SqlField("short", episodes.synopsisShort),
                    SqlField("long", episodes.synopsisLong)
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
