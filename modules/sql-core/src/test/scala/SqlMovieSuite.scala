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

trait SqlMovieSuite extends CatsEffectSuite {

  def mapping: Mapping[IO]

  test("query with UUID argument and custom scalar results") {
    val query = """
      query {
        movieById(id: "6a7837fc-b463-4d32-b628-0f4b3065cb21") {
          id
          title
          genre
          releaseDate
          showTime
          nextShowing
          duration
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "movieById" : {
            "id" : "6a7837fc-b463-4d32-b628-0f4b3065cb21",
            "title" : "Celine et Julie Vont en Bateau",
            "genre" : "DRAMA",
            "releaseDate" : "1974-10-07",
            "showTime" : "19:35:00",
            "nextShowing" : "2020-05-22T19:35:00Z",
            "duration" : "PT3H25M"
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with mapped enum argument") {
    val query = """
      query {
        moviesByGenre(genre: COMEDY) {
          title
          genre
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesByGenre" : [
            {
              "title" : "Daisies",
              "genre" : "COMEDY"
            },
            {
              "title" : "Weekend",
              "genre" : "COMEDY"
            },
            {
              "title" : "Zazie dans le Métro",
              "genre" : "COMEDY"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with LocalDate argument") {
    val query = """
      query {
        moviesReleasedBetween(from: "1970-01-01", to: "1980-01-01") {
          title
          releaseDate
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesReleasedBetween" : [
            {
              "title" : "Duelle",
              "releaseDate" : "1975-09-15"
            },
            {
              "title" : "Stalker",
              "releaseDate" : "1979-05-13"
            },
            {
              "title" : "Celine et Julie Vont en Bateau",
              "releaseDate" : "1974-10-07"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with Duration argument") {
    val query = """
      query {
        moviesLongerThan(duration: "PT3H") {
          title
          duration
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesLongerThan" : [
            {
              "title" : "Celine et Julie Vont en Bateau",
              "duration" : "PT3H25M"
            },
            {
              "title" : "L'Amour fou",
              "duration" : "PT4H12M"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with LocalTime argument") {
    val query = """
      query {
        moviesShownLaterThan(time: "21:00:00") {
          title
          showTime
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesShownLaterThan" : [
            {
              "title" : "Daisies",
              "showTime" : "21:30:00"
            },
            {
              "title" : "Weekend",
              "showTime" : "22:30:00"
            },
            {
              "title" : "L'Amour fou",
              "showTime" : "21:00:00"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with OffsetDateTime argument") {
    val query = """
      query {
        moviesShownBetween(from: "2020-05-01T10:30:00Z", to: "2020-05-19T18:00:00Z") {
          title
          nextShowing
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesShownBetween" : [
            {
              "title" : "Stalker",
              "nextShowing" : "2020-05-19T15:30:00Z"
            },
            {
              "title" : "Daisies",
              "nextShowing" : "2020-05-15T21:30:00Z"
            },
            {
              "title" : "Le Pont du Nord",
              "nextShowing" : "2020-05-11T20:45:00Z"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with computed field") {
    val query = """
      query {
        moviesShownBetween(from: "2020-05-01T10:30:00Z", to: "2020-05-19T18:00:00Z") {
          title
          nextShowing
          duration
          nextEnding
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesShownBetween" : [
            {
              "title" : "Stalker",
              "nextShowing" : "2020-05-19T15:30:00Z",
              "duration" : "PT2H41M",
              "nextEnding" : "2020-05-19T18:11:00Z"
            },
            {
              "title" : "Daisies",
              "nextShowing" : "2020-05-15T21:30:00Z",
              "duration" : "PT1H16M",
              "nextEnding" : "2020-05-15T22:46:00Z"
            },
            {
              "title" : "Le Pont du Nord",
              "nextShowing" : "2020-05-11T20:45:00Z",
              "duration" : "PT2H7M",
              "nextEnding" : "2020-05-11T22:52:00Z"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with standalone computed field") {
    val query = """
      query {
        moviesShownBetween(from: "2020-05-01T10:30:00Z", to: "2020-05-19T18:00:00Z") {
          title
          nextEnding
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesShownBetween" : [
            {
              "title" : "Stalker",
              "nextEnding" : "2020-05-19T18:11:00Z"
            },
            {
              "title" : "Daisies",
              "nextEnding" : "2020-05-15T22:46:00Z"
            },
            {
              "title" : "Le Pont du Nord",
              "nextEnding" : "2020-05-11T22:52:00Z"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with computed attribute") {
    val query = """
      query {
        longMovies {
          title
          duration
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "longMovies" : [
            {
              "title" : "Celine et Julie Vont en Bateau",
              "duration" : "PT3H25M"
            },
            {
              "title" : "L'Amour fou",
              "duration" : "PT4H12M"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with standalone computed attribute") {
    val query = """
      query {
        longMovies {
          title
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "longMovies" : [
            {
              "title" : "Celine et Julie Vont en Bateau"
            },
            {
              "title" : "L'Amour fou"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with enum list argument") {
    val query = """
      query {
        moviesByGenres(genres: [COMEDY, ACTION]) {
          title
          genre
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesByGenres" : [
            {
              "title" : "Zazie dans le Métro",
              "genre" : "COMEDY"
            },
            {
              "title" : "Alphaville",
              "genre" : "ACTION"
            },
            {
              "title" : "Weekend",
              "genre" : "COMEDY"
            },
            {
              "title" : "Daisies",
              "genre" : "COMEDY"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with arrays") {
    val query = """
      query {
        movieById(id: "6a7837fc-b463-4d32-b628-0f4b3065cb21") {
          categories
          features
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "movieById" : {
            "categories" : [
              "drama",
              "comedy"
            ],
            "features" : [
              "HD",
              "HLS"
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with bogus hidden attribute") {
    val query = """
      query {
        moviesByGenre(genre: COMEDY) {
          title
          isLong
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "No field 'isLong' for type Movie"
          }
        ]
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with set encoded as a bitfield") {
    val query = """
      query {
        allMovies {
          title
          tags
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "allMovies" : [
            {
              "title" : "Le Pont du Nord",
              "tags" : [
                "tag1",
                "tag2"
              ]
            },
            {
              "title" : "Last Year at Marienbad",
              "tags" : [
                "tag1",
                "tag3"
              ]
            },
            {
              "title" : "Daisies",
              "tags" : [
                "tag2",
                "tag3"
              ]
            },
            {
              "title" : "Celine et Julie Vont en Bateau",
              "tags" : [
                "tag1"
              ]
            },
            {
              "title" : "Stalker",
              "tags" : [
                "tag1",
                "tag2",
                "tag3"
              ]
            },
            {
              "title" : "Weekend",
              "tags" : [
                "tag2"
              ]
            },
            {
              "title" : "Zazie dans le Métro",
              "tags" : [
                "tag1",
                "tag2"
              ]
            },
            {
              "title" : "L'Amour fou",
              "tags" : [
                "tag2"
              ]
            },
            {
              "title" : "Duelle",
              "tags" : [
                "tag1"
              ]
            },
            {
              "title" : "Alphaville",
              "tags" : [
                "tag3"
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
