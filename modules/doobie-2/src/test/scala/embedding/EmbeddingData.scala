// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package embedding

import cats.effect.Sync
import edu.gemini.grackle._
import edu.gemini.grackle.doobie._
import _root_.doobie.util.meta.Meta
import _root_.doobie.util.transactor.Transactor

trait EmbeddingMapping[F[_]] extends DoobieMapping[F] {
  val schema =
    Schema(
      """
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
    ).right.get

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
            SqlField("title", ColumnRef("films", "title", Meta[String]), key = true),
            SqlObject("synopses")
          )
      ),
      ObjectMapping(
        tpe = SeriesType,
        fieldMappings =
          List(
            SqlField("title", ColumnRef("series", "title", Meta[String]), key = true),
            SqlObject("synopses"),
            SqlObject("episodes", Join(ColumnRef("series", "title", Meta[String]), ColumnRef("episodes2", "series_title", Meta[String]))),
          )
      ),
      ObjectMapping(
        tpe = EpisodeType,
        fieldMappings =
          List(
            SqlField("title", ColumnRef("episodes2", "title", Meta[String]), key = true),
            SqlObject("synopses"),
            SqlAttribute("series_title", ColumnRef("episodes2", "series_title", Meta[String]))
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
                    SqlField("short", ColumnRef("films", "synopsis_short", Meta[String])),
                    SqlField("long", ColumnRef("films", "synopsis_long", Meta[String]))
                  )
              ),
            List("series", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SqlField("short", ColumnRef("series", "synopsis_short", Meta[String])),
                    SqlField("long", ColumnRef("series", "synopsis_long", Meta[String]))
                  )
              ),
            List("episodes", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SqlField("short", ColumnRef("episodes2", "synopsis_short", Meta[String])),
                    SqlField("long", ColumnRef("episodes2", "synopsis_long", Meta[String]))
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
