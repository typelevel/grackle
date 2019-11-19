// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import edu.gemini.grackle._
import Query._, Binding._, Predicate._
import QueryInterpreter.mkErrorResult

import CountryData._
import ComposedSchema._

final class ComposedSpec extends CatsSuite {
  val currencyElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("currency", List(StringBinding("code", code)), child) =>
        Wrap("currency", Unique(FieldEquals("code", code), child)).rightIor
    }
  ))

  val countryElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(StringBinding("code", code)), child) =>
        Wrap("country", Unique(FieldEquals("code", code), child)).rightIor
      case Select("countries", _, child) =>
        Wrap("countries", child).rightIor
    }
  ))

  val countryCurrencyElaborator =  new SelectElaborator(Map(
    QueryType -> {
      case Select("currency", List(StringBinding("code", code)), child) =>
        Wrap("currency", Unique(FieldEquals("code", code), child)).rightIor
      case Select("country", List(StringBinding("code", code)), child) =>
        Wrap("country", Unique(FieldEquals("code", code), child)).rightIor
      case Select("countries", _, child) =>
        Wrap("countries", child).rightIor
    }
  ))

  val dummyJoin = (_: Cursor, q: Query) => q.rightIor

  val countryCurrencyJoin = (c: Cursor, q: Query) =>
    c.focus match {
      case c: Country =>
        Wrap("currency", Unique(FieldEquals("code", c.currencyCode), q)).rightIor
      case _ =>
        mkErrorResult("Bad query")
    }

  val componentElaborator = new ComponentElaborator(Map(
    (QueryType,   "country")   -> ((CountrySchema, CountryType, dummyJoin)),
    (QueryType,   "currency")  -> ((CurrencySchema, CurrencyType, dummyJoin)),
    (QueryType,   "countries") -> ((CountrySchema, CountryType, dummyJoin)),
    (CountryType, "currency")  -> ((CurrencySchema, CurrencyType, countryCurrencyJoin))
  ))

  test("simple currency query") {
    val query = """
      query {
        currency(code: "GBP") {
          code
          exchangeRate
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "currency": {
            "code": "GBP",
            "exchangeRate": 1.25
          }
        }
      }
    """

    val compiledQuery = Compiler.compileText(query)
    val elaboratedQuery = compiledQuery.flatMap(currencyElaborator(_, QueryType)).right.get

    val res = CurrencyQueryInterpreter.run(elaboratedQuery)
    //println(res)

    assert(res == expected)
  }

  test("simple country query") {
    val query = """
      query {
        country(code: "GBR") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "country": {
            "name": "United Kingdom"
          }
        }
      }
    """

    val compiledQuery = Compiler.compileText(query)
    val elaboratedQuery = compiledQuery.flatMap(countryElaborator(_, QueryType)).right.get

    val res = CountryQueryInterpreter.run(elaboratedQuery)
    //println(res)

    assert(res == expected)
  }

  test("simple nested query") {
    val query = """
      query {
        country(code: "GBR") {
          name
          currency {
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
            "currency": {
              "code": "GBP",
              "exchangeRate": 1.25
            }
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

    val res = CountryCurrencyQueryInterpreter.run(mappedQuery)
    //println(res)

    assert(res == expected)
  }

  test("simple multiple nested query") {
    val query = """
      query {
        countries {
          name
          currency {
            code
            exchangeRate
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "countries" : [
            {
              "name" : "Germany",
              "currency" : {
                "code" : "EUR",
                "exchangeRate" : 1.12
              }
            },
            {
              "name" : "France",
              "currency" : {
                "code" : "EUR",
                "exchangeRate" : 1.12
              }
            },
            {
              "name" : "United Kingdom",
              "currency" : {
                "code" : "GBP",
                "exchangeRate" : 1.25
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

    val res = CountryCurrencyQueryInterpreter.run(mappedQuery)
    //println(res)

    assert(res == expected)
  }
}
