// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package embedding

import cats.effect.Sync
import  _root_.skunk.codec.all._
import edu.gemini.grackle._, skunk._
import cats.effect.Resource
import _root_.skunk.Session

trait EmbeddingMapping[F[_]] extends SkunkMapping[F] {
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
            SkunkRoot("films"),
            SkunkRoot("series")
          )
      ),
      ObjectMapping(
        tpe = FilmType,
        fieldMappings =
          List(
            SkunkField("title", ColumnRef("films", "title", text), key = true),
            SkunkObject("synopses", Subobject(Nil))
          )
      ),
      ObjectMapping(
        tpe = SeriesType,
        fieldMappings =
          List(
            SkunkField("title", ColumnRef("series", "title", text), key = true),
            SkunkObject("synopses", Subobject(Nil)),
            SkunkObject("episodes", Subobject(List(Join(ColumnRef("series", "title", text), ColumnRef("episodes2", "series_title", text)))))
          )
      ),
      ObjectMapping(
        tpe = EpisodeType,
        fieldMappings =
          List(
            SkunkField("title", ColumnRef("episodes2", "title", text), key = true),
            SkunkObject("synopses", Subobject(Nil)),
            SkunkAttribute("series_title", ColumnRef("episodes2", "series_title", text), text)
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
                    SkunkField("short", ColumnRef("films", "synopsis_short", text.opt)),
                    SkunkField("long", ColumnRef("films", "synopsis_long", text.opt))
                  )
              ),
            List("series", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SkunkField("short", ColumnRef("series", "synopsis_short", text.opt)),
                    SkunkField("long", ColumnRef("series", "synopsis_long", text.opt))
                  )
              ),
            List("episodes", "synopses") ->
              ObjectMapping(
                tpe = SynopsesType,
                fieldMappings =
                  List(
                    SkunkField("short", ColumnRef("episodes2", "synopsis_short", text.opt)),
                    SkunkField("long", ColumnRef("episodes2", "synopsis_long", text.opt))
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
