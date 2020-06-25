// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package scalars

import io.circe.literal.JsonStringContext

import utils.DatabaseSuite

final class MovieSpec extends DatabaseSuite {
  lazy val mapping = MovieMapping.fromTransactor(xa)

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

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
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

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
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

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
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

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
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

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("query with ZonedDateTime argument") {
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

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
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

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
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

    val compiledQuery = mapping.compiler.compile(query).right.get
    val res = mapping.interpreter.run(compiledQuery, mapping.schema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }
}
