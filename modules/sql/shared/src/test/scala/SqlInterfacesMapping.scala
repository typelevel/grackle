// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.sql.test

import cats.kernel.Eq
import io.circe.Encoder

import grackle._
import syntax._
import Predicate._
import Query._
import QueryCompiler._

trait SqlInterfacesMapping[F[_]] extends SqlTestMapping[F] { self =>

  def entityType: Codec

  object entities extends TableDef("entities") {
    val id                     = col("id", text)
    val entityType             = col("entity_type", self.entityType)
    val title                  = col("title", nullable(text))
    val filmRating             = col("film_rating", nullable(text))
    val filmLabel              = col("film_label", nullable(int4))
    val seriesNumberOfEpisodes = col("series_number_of_episodes", nullable(int4))
    val seriesLabel            = col("series_label", nullable(text))
    val synopsisShort          = col("synopsis_short", nullable(text))
    val synopsisLong           = col("synopsis_long", nullable(text))
  }

  object episodes extends TableDef("episodes") {
    val id                     = col("id", text)
    val title                  = col("title", nullable(text))
    val seriesId               = col("series_id", text)
    val synopsisShort          = col("synopsis_short", nullable(text))
    val synopsisLong           = col("synopsis_long", nullable(text))
  }

  val schema =
    schema"""
      type Query {
        entities: [Entity!]!
        films: [Film!]!
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
            SqlObject("entities"),
            SqlObject("films"),
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
            List("films", "synopses") ->
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

  lazy val entityTypeDiscriminator = new SqlDiscriminator {
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
        Some(Eql(EType / "entityType", Const(tpe)))

      subtpe match {
        case FilmType => mkPredicate(EntityType.Film)
        case SeriesType => mkPredicate(EntityType.Series)
        case _ => None
      }
    }
  }

  override val selectElaborator = SelectElaborator {
    case (QueryType, "films", Nil) =>
      Elab.transformChild(child => Filter(Eql[EntityType](FilmType / "entityType", Const(EntityType.Film)), child))
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

    implicit val entityTypeEncoder: io.circe.Encoder[EntityType] =
      Encoder[String].contramap(_ match {
        case Film => "FILM"
        case Series => "SERIES"
      })

    def fromInt(i: Int): EntityType =
      (i: @unchecked) match {
        case 1 => Film
        case 2 => Series
      }

    def toInt(e: EntityType): Int =
      e match {
        case Film  => 1
        case Series => 2
      }
  }
}
