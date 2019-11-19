// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Id
import cats.implicits._

import edu.gemini.grackle._
import Query._
import QueryInterpreter.mkErrorResult

import ComposedSchema._

object CountryCurrencyQueryInterpreter extends
  ComposedQueryInterpreter[Id](ComposedSchema,
    Map(
      CountrySchema  -> CountryQueryInterpreter,
      CurrencySchema -> CurrencyQueryInterpreter
    )
  )

object CurrencyData {
  case class Currency(
    code: String,
    exchangeRate: Double
  )

  val EUR = Currency("EUR", 1.12)
  val GBP = Currency("GBP", 1.25)

  val currencies = List(EUR, GBP)
}

object CurrencyQueryInterpreter extends DataTypeQueryInterpreter[Id](ComposedSchema) {
  import CurrencyData._

  def rootCursor(query: Query): Result[(Type, Cursor)] =
    query match {
      case Wrap("currency", _) => (NullableType(CurrencyType), CurrencyCursor(currencies)).rightIor
      case _ => mkErrorResult("Bad query")
    }
}

case class CurrencyCursor(focus: Any) extends DataTypeCursor {
  import CurrencyData._

  def mkCursor(focus: Any): Cursor = CurrencyCursor(focus)

  def hasField(fieldName: String): Boolean = (focus, fieldName) match {
    case (_: Currency, "code" | "exchangeRate") => true
    case _ => false
  }

  def field(fieldName: String, args: Map[String, Any]): Result[Cursor] = (focus, fieldName) match {
    case (c: Currency, "code") => mkCursor(c.code).rightIor
    case (c: Currency, "exchangeRate") => mkCursor(c.exchangeRate).rightIor
    case _ => mkErrorResult(s"No field '$fieldName'")
  }
}

object CountryData {
  case class Country(
    code: String,
    name: String,
    currencyCode: String
  )

  val DEU = Country("DEU", "Germany", "EUR")
  val FRA = Country("FRA", "France", "EUR")
  val GBR = Country("GBR", "United Kingdom", "GBP")

  val countries = List(DEU, FRA, GBR)
}

object CountryQueryInterpreter extends DataTypeQueryInterpreter[Id](ComposedSchema) {
  import CountryData._

  def rootCursor(query: Query): Result[(Type, Cursor)] =
    query match {
      case Wrap("country", _) => (NullableType(CountryType), CountryCursor(countries)).rightIor
      case Wrap("countries", _) => (ListType(CountryType), CountryCursor(countries)).rightIor
      case _ => mkErrorResult("Bad query")
    }
}

case class CountryCursor(focus: Any) extends DataTypeCursor {
  import CountryData._

  def mkCursor(focus: Any): Cursor = CountryCursor(focus)

  def hasField(fieldName: String): Boolean = (focus, fieldName) match {
    case (_: Country, "code" | "name") => true
    case _ => false
  }

  def field(fieldName: String, args: Map[String, Any]): Result[Cursor] = (focus, fieldName) match {
    case (c: Country, "code") => mkCursor(c.code).rightIor
    case (c: Country, "name") => mkCursor(c.name).rightIor
    case _ => mkErrorResult(s"No field '$fieldName'")
  }
}
