// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package embedding

import cats.effect.Sync
import  _root_.skunk.codec.all._
import edu.gemini.grackle._, skunk._, syntax._
import cats.effect.Resource
import _root_.skunk.Session

trait EmbeddingMapping[F[_]] extends SkunkMapping[F] {

  object films extends TableDef("films") {
    val title = col("title", text)
    val synopsisShort = col("synopsis_short", text.opt)
    val synopsisLong = col("synopsis_long", text.opt)
  }

  object series extends TableDef("series") {
    val title = col("title", text)
    val synopsisShort = col("synopsis_short", text.opt)
    val synopsisLong = col("synopsis_long", text.opt)
  }

  object episodes extends TableDef("episodes2") {
    val title = col("title", text)
    val seriesTitle = col("series_title", text.opt)
    val synopsisShort = col("synopsis_short", text.opt)
    val synopsisLong = col("synopsis_long", text.opt)
  }

  val schema =
    schema"""
      type Query {
        films: [Film!]!
        series: [Series!]!
        episodes: [Episode!]!
      }
      type Film {
        title: String!
        synopses: Synopses!
      }
      type Series {
        title: String!
        synopses: Synopses!
        episodes: [Episode!]!
      }
      type Episode {
        title: String!
        synopses: Synopses!
      }
      type Synopses {
        short: String
        long: String
      }
    """

  val QueryType = schema.ref("Query")
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
            SqlRoot("films"),
            SqlRoot("series")
          )
      ),
      ObjectMapping(
        tpe = FilmType,
        fieldMappings =
          List(
            SqlField("title", films.title, key = true),
            SqlObject("synopses")
          )
      ),
      ObjectMapping(
        tpe = SeriesType,
        fieldMappings =
          List(
            SqlField("title", series.title, key = true),
            SqlObject("synopses"),
            SqlObject("episodes", Join(series.title, episodes.seriesTitle)),
          )
      ),
      ObjectMapping(
        tpe = EpisodeType,
        fieldMappings =
          List(
            SqlField("title", episodes.title, key = true),
            SqlObject("synopses"),
            SqlField("series_title", episodes.seriesTitle, hidden = true)
          )
      ),
      PrefixedMapping(
        tpe = SynopsesType,
        mappings =
          List(
            List("films", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SqlField("short", films.synopsisShort),
                    SqlField("long", films.synopsisLong)
                  )
              ),
            List("series", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SqlField("short", series.synopsisShort),
                    SqlField("long", series.synopsisLong)
                  )
              ),
            List("episodes", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SqlField("short", episodes.synopsisShort),
                    SqlField("long", episodes.synopsisLong)
                  )
              )
          )
      )
    )
}

object EmbeddingMapping extends SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with EmbeddingMapping[F]

}
