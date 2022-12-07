// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generi

import edu.gemini.grackle.generic.GenericMapping
import edu.gemini.grackle.syntax._

import cats.effect.IO
import cats.tests.CatsSuite
import cats.effect.unsafe.implicits.global

import fs2.concurrent.SignallingRef
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._

final case class Article(slug: String, title: String)

object ArticleMapping {
  def articleMapping(ref: SignallingRef[IO, List[Article]]): GenericMapping[IO] = new GenericMapping[IO] {
    val schema =
      schema"""
        type Query {
          articles: [Article!]!
        }
        type Article {
          slug: String!
          title: String!
        }
      """

    val QueryType   = schema.ref("Query")
    val ArticleType = schema.ref("Article")
  
    implicit val enc: Encoder[Article] = Encoder.instance(x => Json.obj("slug" -> x.slug.asJson, "title" -> x.title.asJson))
    implicit val cb: CursorBuilder[Article] = CursorBuilder.deriveLeafCursorBuilder(ArticleType)

    val typeMappings =
      List(
        ObjectMapping(
          QueryType, 
          List(RootEffect.computeCursor("articles")((_, tpe, env) => ref.get.map(data => genericCursor(tpe, env, data))))
        )
      )
  }
}

final class ArticleSpec extends CatsSuite {
  test("effectful query of generic cursor") {
    val data: List[Article] = 
      List(Article("retired-vegetable", "A has bean"))

    val prog: IO[Json] =
      for {
        ref <- SignallingRef[IO, List[Article]](data)
        map  = ArticleMapping.articleMapping(ref)
        r1  <- map.compileAndRunOne("query { articles { slug title } }")
      } yield r1

    assert(prog.unsafeRunSync() ==
      json"""
        {
          "data" : {
            "articles" : [
              {
                "slug": "retired-vegetable",
                "title": "A has bean"
              }
            ]
          }
        }
      """
    )
  }
}
