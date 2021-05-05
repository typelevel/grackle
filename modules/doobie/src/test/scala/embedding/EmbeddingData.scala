// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package embedding

import cats.effect.Sync
import _root_.doobie.util.meta.Meta
import _root_.doobie.util.transactor.Transactor

import edu.gemini.grackle._
import edu.gemini.grackle.doobie._
import edu.gemini.grackle.syntax._

trait EmbeddingMapping[F[_]] extends DoobieMapping[F] {

  object films extends TableDef("films") {
    val title = col("title", Meta[String])
    val synopsisShort = col("synopsis_short", Meta[String], true)
    val synopsisLong = col("synopsis_long", Meta[String], true)
  }

  object series extends TableDef("series") {
    val title = col("title", Meta[String])
    val synopsisShort = col("synopsis_short", Meta[String], true)
    val synopsisLong = col("synopsis_long", Meta[String], true)
  }

  object episodes extends TableDef("episodes2") {
    val title = col("title", Meta[String])
    val seriesTitle = col("series_title", Meta[String], true)
    val synopsisShort = col("synopsis_short", Meta[String], true)
    val synopsisLong = col("synopsis_long", Meta[String], true)
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

object EmbeddingMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with EmbeddingMapping[F]

}
