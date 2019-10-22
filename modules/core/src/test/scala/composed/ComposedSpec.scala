// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import edu.gemini.grackle._

final class ComposedSpec extends CatsSuite {
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

    val compiledQuery = Compiler.compileText(query).get
    val res = CurrencyQueryInterpreter.run(compiledQuery)
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

    val compiledQuery = Compiler.compileText(query).get
    val res = CountryQueryInterpreter.run(compiledQuery)
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
              "exchangeRate": "1.25"
            }
          }
        }
      }
    """

    val compiledQuery = Compiler.compileText(query).get
    val res = ComposedQueryInterpreter.run(compiledQuery)
    //println(res)

    assert(res == expected)
  }
}
