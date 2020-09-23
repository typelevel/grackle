// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package embedding

import io.circe.literal.JsonStringContext

import utils.DatabaseSuite

final class EmbeddingSpec extends DatabaseSuite {
  lazy val mapping = EmbeddingMapping.fromTransactor(xa)

  test("simple embedded query (1)") {
    val query = """
      query {
        films {
          title
          synopses {
            short
            long
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "films" : [
            {
              "title" : "Film 1",
              "synopses" : {
                "short" : "Short film 1",
                "long" : "Long film 1"
              }
            },
            {
              "title" : "Film 2",
              "synopses" : {
                "short" : "Short film 2",
                "long" : "Long film 2"
              }
            },
            {
              "title" : "Film 3",
              "synopses" : {
                "short" : "Short film 3",
                "long" : "Long film 3"
              }
            }
          ]
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("simple embedded query (2)") {
    val query = """
      query {
        series {
          title
          synopses {
            short
            long
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "series" : [
            {
              "title" : "Series 1",
              "synopses" : {
                "short" : "Short series 1",
                "long" : "Long series 1"
              }
            },
            {
              "title" : "Series 2",
              "synopses" : {
                "short" : "Short series 2",
                "long" : "Long series 2"
              }
            },
            {
              "title" : "Series 3",
              "synopses" : {
                "short" : "Short series 3",
                "long" : "Long series 3"
              }
            }
          ]
        }
      }
    """

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }
}
