// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import scala.concurrent.ExecutionContext

import cats.data.Ior
import cats.effect.{ ContextShift, IO }
import cats.tests.CatsSuite
import doobie.Transactor
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.circe.literal.JsonStringContext

import edu.gemini.grackle._
import ComponentElaborator.Mapping
import Query._, Binding._, Predicate._
import QueryInterpreter.mkErrorResult
import doobie.DoobiePredicate._

import ComposedWorldSchema._

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

  val countryCurrencyElaborator =  new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(StringBinding("code", code)), child) =>
        Wrap("country", Unique(AttrEquals("code", code), child)).rightIor
      case Select("countries", _, child) =>
        Wrap("countries", child).rightIor
      case Select("cities", List(StringBinding("namePattern", namePattern)), child) =>
        Wrap("cities", Filter(FieldLike("name", namePattern, true), child)).rightIor
    }
  ))

  val countryCurrencyJoin = (c: Cursor, q: Query) =>
    c.attribute("code") match {
      case Ior.Right(countryCode: String) =>
        Wrap("currencies", Filter(FieldEquals("countryCode", countryCode), q)).rightIor
      case _ => mkErrorResult("Bad query")
    }

  val componentElaborator = ComponentElaborator(
    Mapping(QueryType, "country", WorldSchema),
    Mapping(QueryType, "countries", WorldSchema),
    Mapping(QueryType, "cities", WorldSchema),
    Mapping(CountryType, "currencies", CurrencySchema, countryCurrencyJoin)
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

    val mappedQuery =
      (for {
        compiled <- Compiler.compileText(query)
        elaborated <- countryCurrencyElaborator(compiled, QueryType)
        mapped <- componentElaborator(elaborated, QueryType)
      } yield mapped).right.get

    val res = CountryCurrencyQueryInterpreter.fromTransactor(xa).run(mappedQuery).unsafeRunSync
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

    val mappedQuery =
      (for {
        compiled <- Compiler.compileText(query)
        elaborated <- countryCurrencyElaborator(compiled, QueryType)
        mapped <- componentElaborator(elaborated, QueryType)
      } yield mapped).right.get

    val res = CountryCurrencyQueryInterpreter.fromTransactor(xa).run(mappedQuery).unsafeRunSync
    //println(res)

    assert(res == expected)
  }
}
