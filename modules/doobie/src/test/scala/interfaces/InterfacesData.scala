// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package interfaces

import cats.effect.Sync
import cats.kernel.Eq
import doobie.util.meta.Meta
import doobie.util.transactor.Transactor
import io.circe.Encoder

import edu.gemini.grackle._
import doobie._
import syntax._
import Path._, Predicate._

trait InterfacesMapping[F[_]] extends DoobieMapping[F] {

  object entities extends TableDef("entities") {
    val id                     = col("id", Meta[String])
    val entityType             = col("entity_type", Meta[EntityType])
    val title                  = col("title", Meta[String], true)
    val filmRating             = col("film_rating", Meta[String], true)
    val filmLabel              = col("film_label", Meta[Int], true)
    val seriesNumberOfEpisodes = col("series_number_of_episodes", Meta[Int], true)
    val seriesLabel            = col("series_label", Meta[String], true)
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
        label: Int
      }
      type Series implements Entity {
        id: ID!
        entityType: EntityType!
        title: String
        synopses: Synopses
        numberOfEpisodes: Int
        episodes: [Episode!]!
        label: String
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
            SqlField("title", entities.title),
            SqlObject("synopses")
          )
      ),
      ObjectMapping(
        tpe = FilmType,
        fieldMappings =
          List(
            SqlField("id", entities.id, key = true),
            SqlField("rating", entities.filmRating),
            SqlField("label", entities.filmLabel)
          )
      ),
      ObjectMapping(
        tpe = SeriesType,
        fieldMappings =
          List(
            SqlField("id", entities.id, key = true),
            SqlField("numberOfEpisodes", entities.seriesNumberOfEpisodes),
            SqlObject("episodes", Join(entities.id, episodes.seriesId)),
            SqlField("label", entities.seriesLabel)
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
                    SqlField("id", entities.id, key = true, hidden = true),
                    SqlField("short", entities.synopsisShort),
                    SqlField("long", entities.synopsisLong)
                  )
              ),
            List("entities", "episodes", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SqlField("id", episodes.id, key = true, hidden = true),
                    SqlField("short", episodes.synopsisShort),
                    SqlField("long", episodes.synopsisLong)
                  )
              )
          )
      ),
      LeafMapping[EntityType](EntityTypeType)
    )

  object entityTypeDiscriminator extends SqlDiscriminator {
    def discriminate(c: Cursor): Result[Type] = {
      for {
        et <- c.fieldAs[EntityType]("entityType")
      } yield et match {
        case EntityType.Film => FilmType
        case EntityType.Series => SeriesType
      }
    }

    def narrowPredicate(subtpe: Type): Option[Predicate] = {
      def mkPredicate(tpe: EntityType): Option[Predicate] =
        Some(Eql(UniquePath(List("entityType")), Const(tpe)))

      subtpe match {
        case FilmType => mkPredicate(EntityType.Film)
        case SeriesType => mkPredicate(EntityType.Series)
        case _ => None
      }
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
