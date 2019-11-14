// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import scala.concurrent.ExecutionContext

import cats.effect.{ ContextShift, IO }
import cats.tests.CatsSuite
import doobie._
import edu.gemini.grackle._
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

    val compiledQuery = Compiler.compileText(query).right.get
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

    val compiledQuery = Compiler.compileText(query).right.get
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

    val compiledQuery = Compiler.compileText(query).right.get
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

    val compiledQuery = Compiler.compileText(query).right.get
    val res = WorldQueryInterpreter.fromTransactor(xa).run(compiledQuery).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  ignore("expanding query") {
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
                    "name" : "Andorra"
                  },
                  {
                    "name" : "Argentina"
                  },
                  {
                    "name" : "Aruba"
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
                    "name" : "El Salvador"
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
                    "name" : "Paraguay"
                  },
                  {
                    "name" : "Peru"
                  },
                  {
                    "name" : "Puerto Rico"
                  },
                  {
                    "name" : "Spain"
                  },
                  {
                    "name" : "Sweden"
                  },
                  {
                    "name" : "United States"
                  },
                  {
                    "name" : "Uruguay"
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

    val compiledQuery = Compiler.compileText(query).right.get
    val res = WorldQueryInterpreter.fromTransactor(xa).run(compiledQuery).unsafeRunSync
    //println(res)

    assert(res == expected)
  }
}
