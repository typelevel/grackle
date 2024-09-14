// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package grackle.sql.test

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlInterfacesSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
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

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with only introspection fields") {
    val query = """
      query {
        entities {
          __typename
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "__typename" : "Film"
            },
            {
              "__typename" : "Film"
            },
            {
              "__typename" : "Film"
            },
            {
              "__typename" : "Series"
            },
            {
              "__typename" : "Series"
            },
            {
              "__typename" : "Series"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with type variant fields in type conditions (1)") {
    val query = """
      query {
        entities {
          id
          entityType
          title
          ... on Film {
            rating
            label
          }
          ... on Series {
            numberOfEpisodes
            alphaLabel:label
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
              "rating" : "PG",
              "label" : 1
            },
            {
              "id" : "2",
              "entityType" : "FILM",
              "title" : "Film 2",
              "rating" : "U",
              "label" : 2
            },
            {
              "id" : "3",
              "entityType" : "FILM",
              "title" : "Film 3",
              "rating" : "15",
              "label" : 3
            },
            {
              "id" : "4",
              "entityType" : "SERIES",
              "title" : "Series 1",
              "numberOfEpisodes" : 5,
              "alphaLabel" : "One"
            },
            {
              "id" : "5",
              "entityType" : "SERIES",
              "title" : "Series 2",
              "numberOfEpisodes" : 6,
              "alphaLabel" : "Two"
            },
            {
              "id" : "6",
              "entityType" : "SERIES",
              "title" : "Series 3",
              "numberOfEpisodes" : 7,
              "alphaLabel" : "Three"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with type variant fields in type conditions (2)") {
    val query = """
      query {
        entities {
          id
          entityType
          title
          ... on Film {
            rating
            label
          }
          ... on Series {
            numberOfEpisodes
            label
          }
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Cannot merge fields named 'label' of distinct leaf types Int, String"
          }
        ]
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with supertype fragment") {
    val query = """
      query {
        films {
          ... on Entity {
            id
          }
          rating
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "films" : [
            {
              "id" : "1",
              "rating" : "PG"
            },
            {
              "id" : "2",
              "rating" : "U"
            },
            {
              "id" : "3",
              "rating" : "15"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with supertype fragment containing subtype refinement") {
    val query = """
      query {
        films {
          ... EntityFields
        }
      }

      fragment EntityFields on Entity {
        id
        ... on Film {
          rating
        }
        ... on Series {
          numberOfEpisodes
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "films" : [
            {
              "id" : "1",
              "rating" : "PG"
            },
            {
              "id" : "2",
              "rating" : "U"
            },
            {
              "id" : "3",
              "rating" : "15"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with supertype fragment containing nested fragment spreads") {
    val query = """
      query {
        films {
          ... EntityFields
        }
      }

      fragment EntityFields on Entity {
        id
        ... FilmFields
        ... SeriesFields
      }

      fragment FilmFields on Film {
        rating
      }

      fragment SeriesFields on Series {
        numberOfEpisodes
      }
    """

    val expected = json"""
      {
        "data" : {
          "films" : [
            {
              "id" : "1",
              "rating" : "PG"
            },
            {
              "id" : "2",
              "rating" : "U"
            },
            {
              "id" : "3",
              "rating" : "15"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with nested inline fragments") {
    val query = """
      query {
        films {
          ... on Entity {
            id
            ... on Film {
              rating
            }
            ... on Series {
              numberOfEpisodes
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "films" : [
            {
              "id" : "1",
              "rating" : "PG"
            },
            {
              "id" : "2",
              "rating" : "U"
            },
            {
              "id" : "3",
              "rating" : "15"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with polymorphic cursor field (1)") {
    val query = """
      query {
        entities {
          id
          ... on Film {
            imageUrl
          }
          ... on Series {
            imageUrl
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "4",
              "imageUrl" : "http://example.com/series/hidden_series1.jpg"
            },
            {
              "id" : "5",
              "imageUrl" : "http://example.com/series/hidden_series2.jpg"
            },
            {
              "id" : "2",
              "imageUrl" : "http://www.example.com/film2.jpg"
            },
            {
              "id" : "3",
              "imageUrl" : "http://www.example.com/film3.jpg"
            },
            {
              "id" : "6",
              "imageUrl" : "http://example.com/series/hidden_series3.jpg"
            },
            {
              "id" : "1",
              "imageUrl" : "http://www.example.com/film1.jpg"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with polymorphic cursor field (2)") {
    val query = """
      query {
        entities {
          id
          imageUrl
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "4",
              "imageUrl" : "http://example.com/series/hidden_series1.jpg"
            },
            {
              "id" : "5",
              "imageUrl" : "http://example.com/series/hidden_series2.jpg"
            },
            {
              "id" : "2",
              "imageUrl" : "http://www.example.com/film2.jpg"
            },
            {
              "id" : "3",
              "imageUrl" : "http://www.example.com/film3.jpg"
            },
            {
              "id" : "6",
              "imageUrl" : "http://example.com/series/hidden_series3.jpg"
            },
            {
              "id" : "1",
              "imageUrl" : "http://www.example.com/film1.jpg"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with polymorphic cursor field (3)") {
    val query = """
      query {
        entities {
          id
          imageUrl
          ... on Film {
            rating
            imageUrl
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "4",
              "imageUrl" : "http://example.com/series/hidden_series1.jpg"
            },
            {
              "id" : "5",
              "imageUrl" : "http://example.com/series/hidden_series2.jpg"
            },
            {
              "id" : "2",
              "imageUrl" : "http://www.example.com/film2.jpg",
              "rating" : "U"
            },
            {
              "id" : "3",
              "imageUrl" : "http://www.example.com/film3.jpg",
              "rating" : "15"
            },
            {
              "id" : "6",
              "imageUrl" : "http://example.com/series/hidden_series3.jpg"
            },
            {
              "id" : "1",
              "imageUrl" : "http://www.example.com/film1.jpg",
              "rating" : "PG"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with polymorphic cursor field (4)") {
    val query = """
      query {
        entities {
          id
          imageUrl
          ... FilmFields
        }
      }

      fragment FilmFields on Film {
        rating
        imageUrl
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "4",
              "imageUrl" : "http://example.com/series/hidden_series1.jpg"
            },
            {
              "id" : "5",
              "imageUrl" : "http://example.com/series/hidden_series2.jpg"
            },
            {
              "id" : "2",
              "imageUrl" : "http://www.example.com/film2.jpg",
              "rating" : "U"
            },
            {
              "id" : "3",
              "imageUrl" : "http://www.example.com/film3.jpg",
              "rating" : "15"
            },
            {
              "id" : "6",
              "imageUrl" : "http://example.com/series/hidden_series3.jpg"
            },
            {
              "id" : "1",
              "imageUrl" : "http://www.example.com/film1.jpg",
              "rating" : "PG"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with polymorphic cursor field (5)") {
    val query = """
      query {
        entities {
          id
          ... on Film {
            rating
            imageUrl
          }
          imageUrl
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "4",
              "imageUrl" : "http://example.com/series/hidden_series1.jpg"
            },
            {
              "id" : "5",
              "imageUrl" : "http://example.com/series/hidden_series2.jpg"
            },
            {
              "id" : "2",
              "rating" : "U",
              "imageUrl" : "http://www.example.com/film2.jpg"
            },
            {
              "id" : "3",
              "rating" : "15",
              "imageUrl" : "http://www.example.com/film3.jpg"
            },
            {
              "id" : "6",
              "imageUrl" : "http://example.com/series/hidden_series3.jpg"
            },
            {
              "id" : "1",
              "rating" : "PG",
              "imageUrl" : "http://www.example.com/film1.jpg"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with polymorphic cursor field (6)") {
    val query = """
      query {
        entities {
          id
          ... FilmFields
          imageUrl
        }
      }

      fragment FilmFields on Film {
        rating
        imageUrl
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "4",
              "imageUrl" : "http://example.com/series/hidden_series1.jpg"
            },
            {
              "id" : "5",
              "imageUrl" : "http://example.com/series/hidden_series2.jpg"
            },
            {
              "id" : "2",
              "rating" : "U",
              "imageUrl" : "http://www.example.com/film2.jpg"
            },
            {
              "id" : "3",
              "rating" : "15",
              "imageUrl" : "http://www.example.com/film3.jpg"
            },
            {
              "id" : "6",
              "imageUrl" : "http://example.com/series/hidden_series3.jpg"
            },
            {
              "id" : "1",
              "rating" : "PG",
              "imageUrl" : "http://www.example.com/film1.jpg"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with polymorphic cursor field (7)") {
    val query = """
      query {
        entities {
          id
          imageUrl
          ... on Series {
            label
            imageUrl
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "4",
              "imageUrl" : "http://example.com/series/hidden_series1.jpg",
              "label" : "One"
            },
            {
              "id" : "5",
              "imageUrl" : "http://example.com/series/hidden_series2.jpg",
              "label" : "Two"
            },
            {
              "id" : "2",
              "imageUrl" : "http://www.example.com/film2.jpg"
            },
            {
              "id" : "3",
              "imageUrl" : "http://www.example.com/film3.jpg"
            },
            {
              "id" : "6",
              "imageUrl" : "http://example.com/series/hidden_series3.jpg",
              "label" : "Three"
            },
            {
              "id" : "1",
              "imageUrl" : "http://www.example.com/film1.jpg"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with polymorphic cursor field (8)") {
    val query = """
      query {
        entities {
          id
          imageUrl
          ... SeriesFields
        }
      }

      fragment SeriesFields on Series {
        label
        imageUrl
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "4",
              "imageUrl" : "http://example.com/series/hidden_series1.jpg",
              "label" : "One"
            },
            {
              "id" : "5",
              "imageUrl" : "http://example.com/series/hidden_series2.jpg",
              "label" : "Two"
            },
            {
              "id" : "2",
              "imageUrl" : "http://www.example.com/film2.jpg"
            },
            {
              "id" : "3",
              "imageUrl" : "http://www.example.com/film3.jpg"
            },
            {
              "id" : "6",
              "imageUrl" : "http://example.com/series/hidden_series3.jpg",
              "label" : "Three"
            },
            {
              "id" : "1",
              "imageUrl" : "http://www.example.com/film1.jpg"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with polymorphic cursor field (9)") {
    val query = """
      query {
        entities {
          id
          ... on Series {
            label
            imageUrl
          }
          imageUrl
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "4",
              "label" : "One",
              "imageUrl" : "http://example.com/series/hidden_series1.jpg"
            },
            {
              "id" : "5",
              "label" : "Two",
              "imageUrl" : "http://example.com/series/hidden_series2.jpg"
            },
            {
              "id" : "2",
              "imageUrl" : "http://www.example.com/film2.jpg"
            },
            {
              "id" : "3",
              "imageUrl" : "http://www.example.com/film3.jpg"
            },
            {
              "id" : "6",
              "label" : "Three",
              "imageUrl" : "http://example.com/series/hidden_series3.jpg"
            },
            {
              "id" : "1",
              "imageUrl" : "http://www.example.com/film1.jpg"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("interface query with polymorphic cursor field (10)") {
    val query = """
      query {
        entities {
          id
          ... SeriesFields
          imageUrl
        }
      }

      fragment SeriesFields on Series {
        label
        imageUrl
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "4",
              "label" : "One",
              "imageUrl" : "http://example.com/series/hidden_series1.jpg"
            },
            {
              "id" : "5",
              "label" : "Two",
              "imageUrl" : "http://example.com/series/hidden_series2.jpg"
            },
            {
              "id" : "2",
              "imageUrl" : "http://www.example.com/film2.jpg"
            },
            {
              "id" : "3",
              "imageUrl" : "http://www.example.com/film3.jpg"
            },
            {
              "id" : "6",
              "label" : "Three",
              "imageUrl" : "http://example.com/series/hidden_series3.jpg"
            },
            {
              "id" : "1",
              "imageUrl" : "http://www.example.com/film1.jpg"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
