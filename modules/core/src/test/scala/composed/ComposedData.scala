// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Id
import cats.implicits._

import edu.gemini.grackle._
import Query._, Binding._
import QueryInterpreter.mkError

object CountryCurrencyQueryInterpreter extends ComposedQueryInterpreter[Id] {
  val schema = ComposedSchema
  import schema._

  import CountryData._

  val currencyMapping =
    ObjectMapping(
      tpe = CurrencyType,
      interpreter = CurrencyQueryInterpreter,
      fieldMappings = Nil
    )

  val countryMapping =
    ObjectMapping(
      tpe = CountryType,
      interpreter = CountryQueryInterpreter,
      fieldMappings =
        List(
          "currency" -> Subobject(currencyMapping, countryCurrencyJoin)
        )
    )

  val queryMapping =
    ObjectMapping(
      tpe = QueryType,
      interpreter = this,
      fieldMappings =
        List(
          "country" -> Subobject(countryMapping),
          "currency" -> Subobject(currencyMapping)
        )
    )

  def countryCurrencyJoin(c: Cursor, q: Query): Query =
    c.focus match {
      case c: Country =>
        Select("currency", List(StringBinding("code", c.currencyCode)), q)
    }

  val objectMappings = List(queryMapping, countryMapping, currencyMapping)
}

object CurrencyData {
  case class Currency(
    code: String,
    exchangeRate: Double
  )

  val GBP = Currency("GBP", 1.25)

  val currencies = List(GBP)
}

object CurrencyQueryInterpreter extends QueryInterpreter[Id] {
  val schema = ComposedSchema
  import schema._

  import CurrencyData._

  def runRootValue(query: Query): Result[ProtoJson] = {
    query match {
      case Select("currency", List(StringBinding("code", code)), child) =>
        runValue(child, NullableType(CurrencyType), CurrencyCursor(currencies.find(_.code == code)))
      case _ => List(mkError("Bad query")).leftIor
    }
  }
}

case class CurrencyCursor(focus: Any) extends DataTypeCursor {
  import CurrencyData._

  def mkCursor(focus: Any): Cursor = CurrencyCursor(focus)

  def hasField(field: String): Boolean = (focus, field) match {
    case (_: Currency, "code" | "exchangeRate") => true
    case _ => false
  }

  def field(field: String, args: Map[String, Any]): Result[Cursor] = (focus, field) match {
    case (c: Currency, "code") => mkCursor(c.code).rightIor
    case (c: Currency, "exchangeRate") => mkCursor(c.exchangeRate).rightIor
    case _ => List(mkError(s"No field '$field'")).leftIor
  }
}

object CountryData {
  case class Country(
    code: String,
    name: String,
    currencyCode: String
  )

  val GBR = Country("GBR", "United Kingdom", "GBP")

  val countries = List(GBR)
}

object CountryQueryInterpreter extends QueryInterpreter[Id] {
  val schema = ComposedSchema
  import schema._

  import CountryData._

  def runRootValue(query: Query): Result[ProtoJson] = {
    query match {
      case Select("country", List(StringBinding("code", code)), child) =>
        runValue(child, NullableType(CountryType), CountryCursor(countries.find(_.code == code)))
      case _ => List(mkError("Bad query")).leftIor
    }
  }
}

case class CountryCursor(focus: Any) extends DataTypeCursor {
  import CountryData._

  def mkCursor(focus: Any): Cursor = CountryCursor(focus)

  def hasField(field: String): Boolean = (focus, field) match {
    case (_: Country, "code" | "name") => true
    case _ => false
  }

  def field(field: String, args: Map[String, Any]): Result[Cursor] = (focus, field) match {
    case (c: Country, "code") => mkCursor(c.code).rightIor
    case (c: Country, "name") => mkCursor(c.name).rightIor
    case _ => List(mkError(s"No field '$field'")).leftIor
  }
}
