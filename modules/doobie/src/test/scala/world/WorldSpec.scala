// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import scala.concurrent.ExecutionContext

import cats.effect.{ ContextShift, IO }
import cats.tests.CatsSuite
import doobie.Transactor
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.circe.literal.JsonStringContext
import io.circe.optics.JsonPath.root

final class WorldSpec extends CatsSuite {
  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  implicit val log = Slf4jLogger.unsafeCreate[IO]

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:world",
    "user",
    "password"
  )

  test("simple query") {
    val query = """
      query {
        countries {
          name
        }
      }
    """

    val expected = 239

    val compiledQuery = WorldQueryCompiler.compile(query).right.get
    val res = WorldQueryInterpreter.fromTransactor(xa).run(compiledQuery).unsafeRunSync
    //println(res)

    val resSize = root.data.countries.arr.getOption(res).map(_.size)

    assert(resSize == Some(expected))
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

    val compiledQuery = WorldQueryCompiler.compile(query).right.get
    val res = WorldQueryInterpreter.fromTransactor(xa).run(compiledQuery).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("simple restricted nested query") {
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

    val compiledQuery = WorldQueryCompiler.compile(query).right.get
    val res = WorldQueryInterpreter.fromTransactor(xa).run(compiledQuery).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("deeply nested query") {
    val query = """
      query {
        cities(namePattern: "Tirana") {
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
        "data": {
          "cities": [
            {
              "name": "Tirana",
              "country": {
                "name": "Albania",
                "cities": [
                  {
                    "name": "Tirana",
                    "country": {
                      "name": "Albania",
                      "cities": [
                        {
                          "name": "Tirana",
                          "country": {
                            "name": "Albania"
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

    val compiledQuery = WorldQueryCompiler.compile(query).right.get
    val res = WorldQueryInterpreter.fromTransactor(xa).run(compiledQuery).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("recursive query (1)") {
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

    val compiledQuery = WorldQueryCompiler.compile(query).right.get
    val res = WorldQueryInterpreter.fromTransactor(xa).run(compiledQuery).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("recursive query (2)") {
    val query = """
      query {
        cities(namePattern: "Brighton") {
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
              "name" : "Brighton",
              "country" : {
                "name" : "United Kingdom",
                "cities" : [
                  {
                    "name" : "London"
                  },
                  {
                    "name" : "Birmingham"
                  },
                  {
                    "name" : "Glasgow"
                  },
                  {
                    "name" : "Liverpool"
                  },
                  {
                    "name" : "Edinburgh"
                  },
                  {
                    "name" : "Sheffield"
                  },
                  {
                    "name" : "Manchester"
                  },
                  {
                    "name" : "Leeds"
                  },
                  {
                    "name" : "Bristol"
                  },
                  {
                    "name" : "Cardiff"
                  },
                  {
                    "name" : "Coventry"
                  },
                  {
                    "name" : "Leicester"
                  },
                  {
                    "name" : "Bradford"
                  },
                  {
                    "name" : "Belfast"
                  },
                  {
                    "name" : "Nottingham"
                  },
                  {
                    "name" : "Kingston upon Hull"
                  },
                  {
                    "name" : "Plymouth"
                  },
                  {
                    "name" : "Stoke-on-Trent"
                  },
                  {
                    "name" : "Wolverhampton"
                  },
                  {
                    "name" : "Derby"
                  },
                  {
                    "name" : "Swansea"
                  },
                  {
                    "name" : "Southampton"
                  },
                  {
                    "name" : "Aberdeen"
                  },
                  {
                    "name" : "Northampton"
                  },
                  {
                    "name" : "Dudley"
                  },
                  {
                    "name" : "Portsmouth"
                  },
                  {
                    "name" : "Newcastle upon Tyne"
                  },
                  {
                    "name" : "Sunderland"
                  },
                  {
                    "name" : "Luton"
                  },
                  {
                    "name" : "Swindon"
                  },
                  {
                    "name" : "Southend-on-Sea"
                  },
                  {
                    "name" : "Walsall"
                  },
                  {
                    "name" : "Bournemouth"
                  },
                  {
                    "name" : "Peterborough"
                  },
                  {
                    "name" : "Brighton"
                  },
                  {
                    "name" : "Blackpool"
                  },
                  {
                    "name" : "Dundee"
                  },
                  {
                    "name" : "West Bromwich"
                  },
                  {
                    "name" : "Reading"
                  },
                  {
                    "name" : "Oldbury/Smethwick (Warley)"
                  },
                  {
                    "name" : "Middlesbrough"
                  },
                  {
                    "name" : "Huddersfield"
                  },
                  {
                    "name" : "Oxford"
                  },
                  {
                    "name" : "Poole"
                  },
                  {
                    "name" : "Bolton"
                  },
                  {
                    "name" : "Blackburn"
                  },
                  {
                    "name" : "Newport"
                  },
                  {
                    "name" : "Preston"
                  },
                  {
                    "name" : "Stockport"
                  },
                  {
                    "name" : "Norwich"
                  },
                  {
                    "name" : "Rotherham"
                  },
                  {
                    "name" : "Cambridge"
                  },
                  {
                    "name" : "Watford"
                  },
                  {
                    "name" : "Ipswich"
                  },
                  {
                    "name" : "Slough"
                  },
                  {
                    "name" : "Exeter"
                  },
                  {
                    "name" : "Cheltenham"
                  },
                  {
                    "name" : "Gloucester"
                  },
                  {
                    "name" : "Saint Helens"
                  },
                  {
                    "name" : "Sutton Coldfield"
                  },
                  {
                    "name" : "York"
                  },
                  {
                    "name" : "Oldham"
                  },
                  {
                    "name" : "Basildon"
                  },
                  {
                    "name" : "Worthing"
                  },
                  {
                    "name" : "Chelmsford"
                  },
                  {
                    "name" : "Colchester"
                  },
                  {
                    "name" : "Crawley"
                  },
                  {
                    "name" : "Gillingham"
                  },
                  {
                    "name" : "Solihull"
                  },
                  {
                    "name" : "Rochdale"
                  },
                  {
                    "name" : "Birkenhead"
                  },
                  {
                    "name" : "Worcester"
                  },
                  {
                    "name" : "Hartlepool"
                  },
                  {
                    "name" : "Halifax"
                  },
                  {
                    "name" : "Woking/Byfleet"
                  },
                  {
                    "name" : "Southport"
                  },
                  {
                    "name" : "Maidstone"
                  },
                  {
                    "name" : "Eastbourne"
                  },
                  {
                    "name" : "Grimsby"
                  },
                  {
                    "name" : "Saint Helier"
                  },
                  {
                    "name" : "Douglas"
                  }
                ]
              }
            }
          ]
        }
      }
    """

    val compiledQuery = WorldQueryCompiler.compile(query).right.get
    val res = WorldQueryInterpreter.fromTransactor(xa).run(compiledQuery).unsafeRunSync
    //println(res)

    assert(res == expected)
  }
}
