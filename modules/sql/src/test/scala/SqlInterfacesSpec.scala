// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import io.circe.literal.JsonStringContext

import org.scalatest.funsuite.AnyFunSuite
import edu.gemini.grackle.QueryExecutor
import cats.effect.IO
import io.circe.Json

trait SqlInterfacesSpec extends AnyFunSuite {
  def mapping: QueryExecutor[IO, Json]

  test("simple interface query") {
    val query = """
      query {
        entities {
          id
          entityType
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
          "entities" : [
            {
              "id" : "1",
              "entityType" : "FILM",
              "title" : "Film 1",
              "synopses" : {
                "short" : "Short film 1",
                "long" : "Long film 1"
              }
            },
            {
              "id" : "2",
              "entityType" : "FILM",
              "title" : "Film 2",
              "synopses" : {
                "short" : "Short film 2",
                "long" : "Long film 2"
              }
            },
            {
              "id" : "3",
              "entityType" : "FILM",
              "title" : "Film 3",
              "synopses" : {
                "short" : "Short film 3",
                "long" : "Long film 3"
              }
            },
            {
              "id" : "4",
              "entityType" : "SERIES",
              "title" : "Series 1",
              "synopses" : {
                "short" : "Short series 1",
                "long" : "Long series 1"
              }
            },
            {
              "id" : "5",
              "entityType" : "SERIES",
              "title" : "Series 2",
              "synopses" : {
                "short" : "Short series 2",
                "long" : "Long series 2"
              }
            },
            {
              "id" : "6",
              "entityType" : "SERIES",
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

    val res = mapping.compileAndRun(query).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("interface query with fragment") {
    val query = """
      query {
        entities {
          id
          entityType
          title
          ... on Film {
            rating
          }
          ... on Series {
            numberOfEpisodes
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "1",
              "entityType" : "FILM",
              "title" : "Film 1",
              "rating" : "PG"
            },
            {
              "id" : "2",
              "entityType" : "FILM",
              "title" : "Film 2",
              "rating" : "U"
            },
            {
              "id" : "3",
              "entityType" : "FILM",
              "title" : "Film 3",
              "rating" : "15"
            },
            {
              "id" : "4",
              "entityType" : "SERIES",
              "title" : "Series 1",
              "numberOfEpisodes" : 5
            },
            {
              "id" : "5",
              "entityType" : "SERIES",
              "title" : "Series 2",
              "numberOfEpisodes" : 6
            },
            {
              "id" : "6",
              "entityType" : "SERIES",
              "title" : "Series 3",
              "numberOfEpisodes" : 7
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("interface query with fragment, no explicit discriminator") {
    val query = """
      query {
        entities {
          id
          title
          ... on Film {
            rating
          }
          ... on Series {
            numberOfEpisodes
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "1",
              "title" : "Film 1",
              "rating" : "PG"
            },
            {
              "id" : "2",
              "title" : "Film 2",
              "rating" : "U"
            },
            {
              "id" : "3",
              "title" : "Film 3",
              "rating" : "15"
            },
            {
              "id" : "4",
              "title" : "Series 1",
              "numberOfEpisodes" : 5
            },
            {
              "id" : "5",
              "title" : "Series 2",
              "numberOfEpisodes" : 6
            },
            {
              "id" : "6",
              "title" : "Series 3",
              "numberOfEpisodes" : 7
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("interface query with introspection") {
    val query = """
      query {
        entities {
          __typename
          entityType
          id
          title
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "__typename" : "Film",
              "entityType" : "FILM",
              "id" : "1",
              "title" : "Film 1"
            },
            {
              "__typename" : "Film",
              "entityType" : "FILM",
              "id" : "2",
              "title" : "Film 2"
            },
            {
              "__typename" : "Film",
              "entityType" : "FILM",
              "id" : "3",
              "title" : "Film 3"
            },
            {
              "__typename" : "Series",
              "entityType" : "SERIES",
              "id" : "4",
              "title" : "Series 1"
            },
            {
              "__typename" : "Series",
              "entityType" : "SERIES",
              "id" : "5",
              "title" : "Series 2"
            },
            {
              "__typename" : "Series",
              "entityType" : "SERIES",
              "id" : "6",
              "title" : "Series 3"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("interface query with subobject list") {
    val query = """
      query {
        entities {
          id
          title
          ... on Film {
            rating
          }

          ... on Series {
            episodes {
              id
              title
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "1",
              "title" : "Film 1",
              "rating" : "PG"
            },
            {
              "id" : "2",
              "title" : "Film 2",
              "rating" : "U"
            },
            {
              "id" : "3",
              "title" : "Film 3",
              "rating" : "15"
            },
            {
              "id" : "4",
              "title" : "Series 1",
              "episodes" : [
                {
                  "id" : "1",
                  "title" : "S1E1"
                },
                {
                  "id" : "2",
                  "title" : "S1E2"
                }
              ]
            },
            {
              "id" : "5",
              "title" : "Series 2",
              "episodes" : [
                {
                  "id" : "3",
                  "title" : "S2E1"
                },
                {
                  "id" : "4",
                  "title" : "S2E2"
                }
              ]
            },
            {
              "id" : "6",
              "title" : "Series 3",
              "episodes" : [
                {
                  "id" : "5",
                  "title" : "S3E1"
                },
                {
                  "id" : "6",
                  "title" : "S3E2"
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("interface query with subobject list and embedded subobjects") {
    val query = """
      query {
        entities {
          ... on Series {
            title
            episodes {
              id
              title
              synopses {
                short
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {},
            {},
            {},
            {
              "title" : "Series 1",
              "episodes" : [
                {
                  "id" : "1",
                  "title" : "S1E1",
                  "synopses" : {
                    "short" : "Short S1E1"
                  }
                },
                {
                  "id" : "2",
                  "title" : "S1E2",
                  "synopses" : {
                    "short" : "Short S1E2"
                  }
                }
              ]
            },
            {
              "title" : "Series 2",
              "episodes" : [
                {
                  "id" : "3",
                  "title" : "S2E1",
                  "synopses" : {
                    "short" : "Short S2E1"
                  }
                },
                {
                  "id" : "4",
                  "title" : "S2E2",
                  "synopses" : {
                    "short" : "Short S2E2"
                  }
                }
              ]
            },
            {
              "title" : "Series 3",
              "episodes" : [
                {
                  "id" : "5",
                  "title" : "S3E1",
                  "synopses" : {
                    "short" : "Short S3E1"
                  }
                },
                {
                  "id" : "6",
                  "title" : "S3E2",
                  "synopses" : {
                    "short" : "Short S3E2"
                  }
                }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync
    //println(res)

    assert(res == expected)
  }
}
