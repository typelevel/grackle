// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package embedding

import cats.effect.Sync
import doobie.Transactor

import edu.gemini.grackle._, doobie._

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
          short: String!
          long: String!
        }
      """
    ).right.get

  val QueryType = schema.ref("Query")
  val FilmType = schema.ref("Film")
  val SeriesType = schema.ref("Series")
  val EpisodeType = schema.ref("Episode")
  val SynopsesType = schema.ref("Synopses")

  import DoobieFieldMapping._

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            DoobieRoot("films"),
            DoobieRoot("series")
          )
      ),
      ObjectMapping(
        tpe = FilmType,
        fieldMappings =
          List(
            DoobieField("title", ColumnRef("films", "title"), key = true),
            DoobieObject("synopses", Subobject(Nil))
          )
      ),
      ObjectMapping(
        tpe = SeriesType,
        fieldMappings =
          List(
            DoobieField("title", ColumnRef("series", "title"), key = true),
            DoobieObject("synopses", Subobject(Nil)),
            DoobieObject("episodes", Subobject(List(Join(ColumnRef("series", "title"), ColumnRef("episodes2", "series_title")))))
          )
      ),
      ObjectMapping(
        tpe = EpisodeType,
        fieldMappings =
          List(
            DoobieField("title", ColumnRef("episodes2", "title"), key = true),
            DoobieObject("synopses", Subobject(Nil)),
            DoobieAttribute[String]("series_title", ColumnRef("episodes2", "series_title"))
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
                    DoobieField("short", ColumnRef("films", "synopsis_short")),
                    DoobieField("long", ColumnRef("films", "synopsis_long"))
                  )
              ),
            List("series", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    DoobieField("short", ColumnRef("series", "synopsis_short")),
                    DoobieField("long", ColumnRef("series", "synopsis_long"))
                  )
              ),
            List("episodes", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    DoobieField("short", ColumnRef("episodes2", "synopsis_short")),
                    DoobieField("long", ColumnRef("episodes2", "synopsis_long"))
                  )
              )
          )
      )
    )
}

object EmbeddingMapping extends DoobieMappingCompanion {
  def mkMapping[F[_] : Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): EmbeddingMapping[F] =
    new DoobieMapping(transactor, monitor) with EmbeddingMapping[F]
}
