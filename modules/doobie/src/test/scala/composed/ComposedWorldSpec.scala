// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import scala.concurrent.ExecutionContext

import cats.effect.{ ContextShift, IO }
import cats.tests.CatsSuite
import doobie.Transactor
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.circe.literal.JsonStringContext

final class ComposedWorldSpec extends CatsSuite {
  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  implicit val log = Slf4jLogger.unsafeCreate[IO]

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:world",
    "user",
    "password"
  )

  test("simple composed query") {
    val query = """
      query {
        country(code: "GBR") {
          name
          currencies {
            code
            exchangeRate
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "name" : "United Kingdom",
            "currencies": [
              {
                "code": "GBP",
                "exchangeRate": 1.25
              }
            ]
          }
        }
      }
    """

    val compiledQuery = ComposedQueryCompiler.compile(query).right.get
    val res = ComposedQueryInterpreter.fromTransactor(xa).run(compiledQuery, ComposedSchema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("simple multiple nested query") {
    val query = """
      query {
        cities(namePattern: "Ame%") {
          name
          country {
            name
            currencies {
              code
              exchangeRate
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data": {
          "cities" : [
            {
              "name" : "Amersfoort",
              "country" : {
                "name" : "Netherlands",
                "currencies" : [
                  {
                    "code" : "EUR",
                    "exchangeRate" : 1.12
                  }
                ]
              }
            },
            {
              "name" : "Americana",
              "country" : {
                "name" : "Brazil",
                "currencies" : [
                  {
                    "code" : "BRL",
                    "exchangeRate" : 0.25
                  }
                ]
              }
            }
          ]
        }
      }
    """

    val compiledQuery = ComposedQueryCompiler.compile(query).right.get
    val res = ComposedQueryInterpreter.fromTransactor(xa).run(compiledQuery, ComposedSchema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }

  test("composed query with introspection") {
    val query = """
      query {
        country(code: "GBR") {
          __typename
          name
          currencies {
            __typename
            code
            exchangeRate
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country" : {
            "__typename" : "Country",
            "name" : "United Kingdom",
            "currencies" : [
              {
                "__typename" : "Currency",
                "code" : "GBP",
                "exchangeRate" : 1.25
              }
            ]
          }
        }
      }
    """

    val compiledQuery = ComposedQueryCompiler.compile(query).right.get
    val res = ComposedQueryInterpreter.fromTransactor(xa).run(compiledQuery, ComposedSchema.queryType).unsafeRunSync
    //println(res)

    assert(res == expected)
  }
}
