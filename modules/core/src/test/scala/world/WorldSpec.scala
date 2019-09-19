// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini
package grackle
package world

import scala.concurrent.ExecutionContext

import cats.effect.{ ContextShift, IO }
import cats.tests.CatsSuite
import doobie._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.circe.literal.JsonStringContext

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

  test("simple restricted query") {
    val query = """
      query {
        country(code: "AFG") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data": {
          "country": {
            "name": "Afghanistan"
          }
        }
      }
    """

    val compiledQuery = Compiler.compileText(query).get
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
                    "language": "Dutch"
                  },
                  {
                    "language": "Fries"
                  },
                  {
                    "language": "Arabic"
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
                    "language": "Portuguese"
                  },
                  {
                    "language": "German"
                  },
                  {
                    "language": "Italian"
                  },
                  {
                    "language": "Japanese"
                  },
                  {
                    "language": "Indian Languages"
                  }
                ]
              }
            }
          ]
        }
      }
    """

    val compiledQuery = Compiler.compileText(query).get
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

    val compiledQuery = Compiler.compileText(query).get
    val res = WorldQueryInterpreter.fromTransactor(xa).run(compiledQuery).unsafeRunSync
    //println(res)

    assert(res == expected)
  }
}
