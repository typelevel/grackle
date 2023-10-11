// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import cats.implicits._
import io.circe.literal._
import munit.CatsEffectSuite

import edu.gemini.grackle._

import grackle.test.GraphQLResponseTests.{assertNoErrorsIO, assertWeaklyEqualIO}

trait SqlWorldSuite extends CatsEffectSuite {

  def mapping: Mapping[IO]

  test("simple query") {
    val query = """
      query {
        countries {
          name
        }
      }
    """

    val expected = 239

    val res = mapping.compileAndRun(query)

    val resSize =
      res.map (_.hcursor
        .downField("data")
        .downField("countries")
        .values.map(_.size))

    assertIO(resSize, Some(expected))
  }

  test("simple restricted query") {
    val query = """
      query {
        country(code: "GBR") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data": {
          "country": {
            "name": "United Kingdom"
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("simple nested query") {
    val query = """
      query {
        cities {
          name
          country {
            name
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertNoErrorsIO(res)
  }

  test("simple restricted nested query (1)") {
    val query = """
      query {
        cities(namePattern: "Ame%") {
          name
          country {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "cities" : [
            {
              "name" : "Amersfoort",
              "country" : {
                "name" : "Netherlands"
              }
            },
            {
              "name" : "Americana",
              "country" : {
                "name" : "Brazil"
              }
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("simple restricted nested query (2)") {
    val query = """
      query {
        cities(namePattern: "Ame%") {
          name
          country {
            name
            languages {
              language
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data": {
          "cities": [
            {
              "name": "Amersfoort",
              "country": {
                "name": "Netherlands",
                "languages": [
                  {
                    "language": "Arabic"
                  },
                  {
                    "language": "Dutch"
                  },
                  {
                    "language": "Fries"
                  },
                  {
                    "language": "Turkish"
                  }
                ]
              }
            },
            {
              "name": "Americana",
              "country": {
                "name": "Brazil",
                "languages": [
                  {
                    "language": "German"
                  },
                  {
                    "language": "Indian Languages"
                  },
                  {
                    "language": "Italian"
                  },
                  {
                    "language": "Japanese"
                  },
                  {
                    "language": "Portuguese"
                  }
                ]
              }
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("multiple aliased root queries") {
    val query = """
      query {
        gbr: country(code: "GBR") {
          name
        }
        fra: country(code: "FRA") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "gbr" : {
            "name" : "United Kingdom"
          },
          "fra" : {
            "name" : "France"
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("recursive query (0)") {
    val query = """
      query {
        cities(namePattern: "Monte-Carlo") {
          name
          country {
            name
            cities {
              name
            }
          }
        }
      }
    """

    val expected = json"""
    {
      "data" : {
        "cities" : [
          {
            "name" : "Monte-Carlo",
            "country" : {
              "name" : "Monaco",
              "cities" : [
                {
                  "name" : "Monte-Carlo"
                },
                {
                  "name" : "Monaco-Ville"
                }
              ]
            }
          }
        ]
      }
    }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("recursive query (1)") {
    val query = """
      query {
        cities(namePattern: "Monte-Carlo") {
          name
          country {
            name
            cities {
              name
              country {
                name
                cities {
                  name
                  country {
                    name
                  }
                }
              }
            }
          }
        }
      }
    """

    val expected = json"""
    {
      "data" : {
        "cities" : [
          {
            "name" : "Monte-Carlo",
            "country" : {
              "name" : "Monaco",
              "cities" : [
                {
                  "name" : "Monte-Carlo",
                  "country" : {
                    "name" : "Monaco",
                    "cities" : [
                      {
                        "name" : "Monte-Carlo",
                        "country" : {
                          "name" : "Monaco"
                        }
                      },
                      {
                        "name" : "Monaco-Ville",
                        "country" : {
                          "name" : "Monaco"
                        }
                      }
                    ]
                  }
                },
                {
                  "name" : "Monaco-Ville",
                  "country" : {
                    "name" : "Monaco",
                    "cities" : [
                      {
                        "name" : "Monte-Carlo",
                        "country" : {
                          "name" : "Monaco"
                        }
                      },
                      {
                        "name" : "Monaco-Ville",
                        "country" : {
                          "name" : "Monaco"
                        }
                      }
                    ]
                  }
                }
              ]
            }
          }
        ]
      }
    }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("recursive query (2)") {
    val query = """
      query {
        country(code: "ESP") {
          name
          languages {
            language
            countries {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "Spain",
            "languages" : [
              {
                "language" : "Basque",
                "countries" : [
                  {
                    "name" : "Spain"
                  }
                ]
              },
              {
                "language" : "Catalan",
                "countries" : [
                  {
                    "name" : "Andorra"
                  },
                  {
                    "name" : "Spain"
                  }
                ]
              },
              {
                "language" : "Galecian",
                "countries" : [
                  {
                    "name" : "Spain"
                  }
                ]
              },
              {
                "language" : "Spanish",
                "countries" : [
                  {
                    "name" : "Aruba"
                  },
                  {
                    "name" : "Andorra"
                  },
                  {
                    "name" : "Argentina"
                  },
                  {
                    "name" : "Belize"
                  },
                  {
                    "name" : "Bolivia"
                  },
                  {
                    "name" : "Canada"
                  },
                  {
                    "name" : "Chile"
                  },
                  {
                    "name" : "Colombia"
                  },
                  {
                    "name" : "Costa Rica"
                  },
                  {
                    "name" : "Cuba"
                  },
                  {
                    "name" : "Dominican Republic"
                  },
                  {
                    "name" : "Ecuador"
                  },
                  {
                    "name" : "Spain"
                  },
                  {
                    "name" : "France"
                  },
                  {
                    "name" : "Guatemala"
                  },
                  {
                    "name" : "Honduras"
                  },
                  {
                    "name" : "Mexico"
                  },
                  {
                    "name" : "Nicaragua"
                  },
                  {
                    "name" : "Panama"
                  },
                  {
                    "name" : "Peru"
                  },
                  {
                    "name" : "Puerto Rico"
                  },
                  {
                    "name" : "Paraguay"
                  },
                  {
                    "name" : "El Salvador"
                  },
                  {
                    "name" : "Sweden"
                  },
                  {
                    "name" : "Uruguay"
                  },
                  {
                    "name" : "United States"
                  },
                  {
                    "name" : "Venezuela"
                  },
                  {
                    "name" : "Virgin Islands, U.S."
                  }
                ]
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("recursive query (3)") {
    val query = """
      query {
        cities(namePattern: "Lausanne") {
          name
          country {
            name
            cities {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "cities" : [
            {
              "name" : "Lausanne",
              "country" : {
                "name" : "Switzerland",
                "cities" : [
                  {
                    "name" : "Zürich"
                  },
                  {
                    "name" : "Geneve"
                  },
                  {
                    "name" : "Basel"
                  },
                  {
                    "name" : "Bern"
                  },
                  {
                    "name" : "Lausanne"
                  }
                ]
              }
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("recursive query (4)") {
    val query = """
      query {
        country(code: "CHE") {
          name
          cities {
            name
            country {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "Switzerland",
            "cities" : [
              {
                "name" : "Zürich",
                "country" : {
                  "name" : "Switzerland"
                }
              },
              {
                "name" : "Geneve",
                "country" : {
                  "name" : "Switzerland"
                }
              },
              {
                "name" : "Basel",
                "country" : {
                  "name" : "Switzerland"
                }
              },
              {
                "name" : "Bern",
                "country" : {
                  "name" : "Switzerland"
                }
              },
              {
                "name" : "Lausanne",
                "country" : {
                  "name" : "Switzerland"
                }
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("recursive query (5)") {
    val query = """
      query {
        language(language: "Estonian") {
          language
          countries {
            name
            languages {
              language
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "language" : {
            "language" : "Estonian",
            "countries" : [
              {
                "name" : "Estonia",
                "languages" : [
                  {
                    "language" : "Belorussian"
                  },
                  {
                    "language" : "Estonian"
                  },
                  {
                    "language" : "Finnish"
                  },
                  {
                    "language" : "Russian"
                  },
                  {
                    "language" : "Ukrainian"
                  }
                ]
              },
              {
                "name" : "Finland",
                "languages" : [
                  {
                    "language" : "Estonian"
                  },
                  {
                    "language" : "Finnish"
                  },
                  {
                    "language" : "Russian"
                  },
                  {
                    "language" : "Saame"
                  },
                  {
                    "language" : "Swedish"
                  }
                ]
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("recursive query (6)") {
    val query = """
      query {
        cities(namePattern: "Monte-Carlo") {
          name
          a: country {
            name
            b: cities {
              name
              c: country {
                name
                d: cities {
                  name
                  e: country {
                    name
                  }
                }
              }
            }
          }
        }
      }
    """

    val expected = json"""
    {
      "data" : {
        "cities" : [
          {
            "name" : "Monte-Carlo",
            "a" : {
              "name" : "Monaco",
              "b" : [
                {
                  "name" : "Monte-Carlo",
                  "c" : {
                    "name" : "Monaco",
                    "d" : [
                      {
                        "name" : "Monte-Carlo",
                        "e" : {
                          "name" : "Monaco"
                        }
                      },
                      {
                        "name" : "Monaco-Ville",
                        "e" : {
                          "name" : "Monaco"
                        }
                      }
                    ]
                  }
                },
                {
                  "name" : "Monaco-Ville",
                  "c" : {
                    "name" : "Monaco",
                    "d" : [
                      {
                        "name" : "Monte-Carlo",
                        "e" : {
                          "name" : "Monaco"
                        }
                      },
                      {
                        "name" : "Monaco-Ville",
                        "e" : {
                          "name" : "Monaco"
                        }
                      }
                    ]
                  }
                }
              ]
            }
          }
        ]
      }
    }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("recursive query (7)") {
    val query = """
      query {
        cities(namePattern: "Monte-Carlo") {
          name
          country {
            cities {
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "cities" : [
            {
              "name" : "Monte-Carlo",
              "country" : {
                "cities" : [
                  {
                    "name" : "Monte-Carlo"
                  },
                  {
                    "name" : "Monaco-Ville"
                  }
                ]
              }
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("country with no cities") {
    val query = """
      query {
        country(code: "ATA") {
          name
          cities {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data": {
          "country": {
            "name": "Antarctica",
            "cities": []
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  // Outer join in which some parents have children and others do not.
  test("countries, some with no cities") {
    val query = """
      query {
        countries {
          name
          cities {
            name
          }
        }
      }
    """

    mapping.compileAndRun(query).map { json =>
      val countries =
        json
          .hcursor
          .downField("data")
          .downField("countries")
          .values
          .map(_.toVector)
          .get

      val map = countries.map(j => j.hcursor.downField("name").as[String].toOption.get -> j.hcursor.downField("cities").values.map(_.size).get).toMap

      assert(map("Kazakstan")  == 21)
      assert(map("Antarctica") == 0)
    }
  }

  test("no such country") {
    val query = """
      query {
        country(code: "XXX") {
          name
          cities {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : null
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("multiple missing countries") {
    val query = """
      query {
        xxx: country(code: "xxx") {
          name
        }
        yyy: country(code: "yyy") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "xxx" : null,
          "yyy" : null
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("nullable column (null)") {
    val query = """
    query {
        country(code: "ANT") {
          name
          indepyear
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "Netherlands Antilles",
            "indepyear" : null
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("nullable column (non-null)") {
    val query = """
      query {
        country(code: "USA") {
          name
          indepyear
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "United States",
            "indepyear" : 1776
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with introspection") {
    val query = """
      query {
        country(code: "GBR") {
          __typename
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "__typename" : "Country",
            "name" : "United Kingdom"
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("structured predicates") {
    val query = """
      query {
        search(minPopulation: 20000000, indepSince: 1980) {
          name
          population
          indepyear
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "search" : [
            {
              "name" : "Russian Federation",
              "population" : 146934000,
              "indepyear" : 1991
            },
            {
              "name" : "Ukraine",
              "population" : 50456000,
              "indepyear" : 1991
            },
            {
              "name" : "Uzbekistan",
              "population" : 24318000,
              "indepyear" : 1991
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("simple query with limit") {
    val query = """
      query {
        countries(limit: 3) {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : [
            {
              "name" : "Aruba"
            },
            {
              "name" : "Afghanistan"
            },
            {
              "name" : "Angola"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("simple query with limit and offset") {
    val query = """
      query {
        countries(limit: 3, offset: 2) {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : [
            {
              "name" : "Angola"
            },
            {
              "name" : "Anguilla"
            },
            {
              "name" : "Albania"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("simple query with limit, filter and ordering") {
    val query = """
      query {
        countries(limit: 3, minPopulation: 1, byPopulation: true) {
          name
          population
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : [
            {
              "name" : "Pitcairn",
              "population" : 50
            },
            {
              "name" : "Cocos (Keeling) Islands",
              "population" : 600
            },
            {
              "name" : "Holy See (Vatican City State)",
              "population" : 1000
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected, strictPaths = List(List("data", "countries")))
  }

  test("query with ordering, ordering field not selected") {
    val query = """
      query {
        countries(limit: 10, byPopulation: true) {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : [
            {
              "name" : "Antarctica"
            },
            {
              "name" : "French Southern territories"
            },
            {
              "name" : "Bouvet Island"
            },
            {
              "name" : "Heard Island and McDonald Islands"
            },
            {
              "name" : "British Indian Ocean Territory"
            },
            {
              "name" : "South Georgia and the South Sandwich Islands"
            },
            {
              "name" : "United States Minor Outlying Islands"
            },
            {
              "name" : "Pitcairn"
            },
            {
              "name" : "Cocos (Keeling) Islands"
            },
            {
              "name" : "Holy See (Vatican City State)"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected, strictPaths = List(List("data", "countries")))
  }

  test("null in a nullable joined column") {
    val query = """
      query {
        city(id: 33) {
          name
          country {
            name
            indepyear
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "city" : {
            "name" : "Willemstad",
            "country" : {
              "name" : "Netherlands Antilles",
              "indepyear" : null
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("multiple nested lists (1)") {
    val query = """
      query {
        language(language: "Icelandic") {
          one: countries {
            cities {
              name
            }
            languages {
              language
            }
          }
          two: countries {
            cities {
              name
            }
            languages {
              language
              one: countries {
                cities {
                  name
                }
                languages {
                  language
                }
              }
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertNoErrorsIO(res)
  }

  test("multiple nested lists (2)") {
    val query = """
      query {
        language(language: "Icelandic") {
          countries {
            cities {
              name
            }
            languages {
              countries {
                cities {
                  name
                }
                languages {
                  language
                }
              }
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertNoErrorsIO(res)
  }

  test("multiple nested lists (3)") {
    val query = """
      query {
        language(language: "Icelandic") {
          countries {
            cities {
              name
            }
            languages {
              countries {
                name
              }
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertNoErrorsIO(res)
  }

  test("non-null filter") {
    val query = """
      query {
        search2(indep: true, limit: 3) {
          name
          indepyear
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "search2" : [
            {
              "name" : "Afghanistan",
              "indepyear" : 1919
            },
            {
              "name" : "Albania",
              "indepyear" : 1912
            },
            {
              "name" : "Angola",
              "indepyear" : 1975
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("null filter") {
    val query = """
      query {
        search2(indep: false, limit: 3) {
          name
          indepyear
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "search2" : [
            {
              "name" : "Netherlands Antilles",
              "indepyear" : null
            },
            {
              "name" : "Anguilla",
              "indepyear" : null
            },
            {
              "name" : "Aruba",
              "indepyear" : null
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("count cities") {
    val query = """
      query {
        country(code: "FIN") {
          numCities
          cities {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "numCities" : 7,
            "cities" : [
              {
                "name" : "Helsinki [Helsingfors]"
              },
              {
                "name" : "Espoo"
              },
              {
                "name" : "Tampere"
              },
              {
                "name" : "Vantaa"
              },
              {
                "name" : "Turku [Ĺbo]"
              },
              {
                "name" : "Oulu"
              },
              {
                "name" : "Lahti"
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("count cities with filter") {
    val query = """
      query {
        country(code: "FIN") {
          numCities(namePattern: "T%")
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "numCities" : 2
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("count with no cities") {
    val query = """
      query {
        country(code: "ATA") {
          numCities
          cities {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data": {
          "country": {
            "numCities" : 0,
            "cities": []
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("count all cities (1)") {
    val query = """
      query {
        countries {
          numCities
          cities {
            name
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    val expected = 4079

    val resTotal =
      res.map(_.hcursor
        .downField("data")
        .downField("countries")
        .values.flatMap(_.toSeq.traverse(_.hcursor.downField("numCities").as[Int]).map(_.sum).toOption)
      )

    assertNoErrorsIO(res) *>
      assertIO(resTotal, Some(expected))
  }

  test("count all cities (2)") {
    val query = """
      query {
        countries {
          numCities
        }
      }
    """

    val res = mapping.compileAndRun(query)

    val expected = 4079

    val resTotal =
      res.map(_.hcursor
        .downField("data")
        .downField("countries")
        .values.flatMap(_.toSeq.traverse(_.hcursor.downField("numCities").as[Int]).map(_.sum).toOption)
      )

    assertNoErrorsIO(res) *>
      assertIO(resTotal, Some(expected))
  }

  test("count all cities with filter") {
    val query = """
      query {
        countries {
          numCities(namePattern: "T%")
        }
      }
    """

    val res = mapping.compileAndRun(query)

    val expected = 255

    val resTotal =
      res.map(_.hcursor
        .downField("data")
        .downField("countries")
        .values.flatMap(_.toSeq.traverse(_.hcursor.downField("numCities").as[Int]).map(_.sum).toOption)
      )

    assertNoErrorsIO(res) *>
      assertIO(resTotal, Some(expected))
  }

  // JS treats floats as doubles
  def isJS = Option(System.getProperty("java.vm.name")).contains("Scala.js")

  test("data types (1)") {
    val query = """
      query {
        country(code: "GBR") {
          name
          continent
          region
          surfacearea
          indepyear
          population
          lifeexpectancy
          gnp
          gnpold
          localname
          governmentform
          headofstate
          capitalId
          code2
          numCities(namePattern: "%")
        }
      }
    """

    val expected = if (isJS)
      json"""
          {
            "data" : {
              "country" : {
                "name" : "United Kingdom",
                "continent" : "Europe",
                "region" : "British Islands",
                "surfacearea" : 242900.0,
                "indepyear" : 1066,
                "population" : 59623400,
                "lifeexpectancy" : 77.69999694824219,
                "gnp" : 1378330.00,
                "gnpold" : 1296830.00,
                "localname" : "United Kingdom",
                "governmentform" : "Constitutional Monarchy",
                "headofstate" : "Elisabeth II",
                "capitalId" : 456,
                "code2" : "GB",
                "numCities" : 81
              }
            }
          }
        """
    else
      json"""
          {
            "data" : {
              "country" : {
                "name" : "United Kingdom",
                "continent" : "Europe",
                "region" : "British Islands",
                "surfacearea" : 242900.0,
                "indepyear" : 1066,
                "population" : 59623400,
                "lifeexpectancy" : 77.7,
                "gnp" : 1378330.00,
                "gnpold" : 1296830.00,
                "localname" : "United Kingdom",
                "governmentform" : "Constitutional Monarchy",
                "headofstate" : "Elisabeth II",
                "capitalId" : 456,
                "code2" : "GB",
                "numCities" : 81
              }
            }
          }
        """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("data types (2)") {
    val query = """
      query {
        city(id: 490) {
          name
          district
          population
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "city" : {
            "name" : "Brighton",
            "district" : "England",
            "population" : 156124
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("data types (3)") {
    val query = """
      query {
        language(language: "Marma") {
          language
          isOfficial
          percentage
        }
      }
    """

    val expected = if (isJS)
      json"""
        {
          "data" : {
            "language" : {
              "language" : "Marma",
              "isOfficial" : false,
              "percentage" : 0.20000000298023224
            }
          }
        }
      """
    else
      json"""
        {
          "data" : {
            "language" : {
              "language" : "Marma",
              "isOfficial" : false,
              "percentage" : 0.2
            }
          }
        }
      """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("root unique of limit 1") {
    val query = """
      query {
        city(id: 490) {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "city" : {
            "name" : "Brighton"
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("nested unique of limit 1") {
    val query = """
      query {
        country(code: "GBR") {
          city(id: 490) {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "city" : {
              "name" : "Brighton"
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("set inclusion") {
    val query = """
      query {
        languages(languages: ["French", "German"]) {
          language
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "languages" : [
            {
              "language" : "German"
            },
            {
              "language" : "French"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("empty set inclusion") {
    val query = """
      query {
        languages(languages: []) {
          language
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "languages" : [
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("top-level count (1)") {
    val query = """
      query {
        numCountries
      }
    """

    val expected = json"""
      {
        "data" : {
          "numCountries" : 239
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("top-level count (2)") {
    val query = """
      query {
        numCountries
        city(id: 490) {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "numCountries" : 239,
          "city" : {
            "name" : "Brighton"
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("top-level count (3)") {
    val query = """
      query {
        numCountries
        country(code: "GBR") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "numCountries" : 239,
          "country" : {
            "name" : "United Kingdom"
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("query with top-level ordering, limit and subobjects") {
    val query = """
      query {
        countries(limit: 3, byPopulation: true) {
          name
          population
          cities {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : [
            {
              "name" : "Bouvet Island",
              "population" : 0,
              "cities" : [
              ]
            },
            {
              "name" : "Antarctica",
              "population" : 0,
              "cities" : [
              ]
            },
            {
              "name" : "French Southern territories",
              "population" : 0,
              "cities" : [
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected, strictPaths = List(List("data", "countries")))
  }
}
