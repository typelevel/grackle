// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Id
import cats.implicits._

import edu.gemini.grackle._
import Query._, Binding._, Predicate._
import QueryCompiler._, ComponentElaborator.Mapping
import QueryInterpreter.mkErrorResult

object CurrencyQueryCompiler extends QueryCompiler(ComposedSchema) {
  val selectElaborator = new SelectElaborator(Map(
    ComposedSchema.tpe("Query").dealias -> {
      case Select("currency", List(StringBinding("code", code)), child) =>
        Wrap("currency", Unique(FieldEquals("code", code), child)).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

object CountryQueryCompiler extends QueryCompiler(ComposedSchema) {
  val selectElaborator = new SelectElaborator(Map(
    ComposedSchema.tpe("Query").dealias -> {
      case Select("country", List(StringBinding("code", code)), child) =>
        Wrap("country", Unique(FieldEquals("code", code), child)).rightIor
      case Select("countries", _, child) =>
        Wrap("countries", child).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

object CountryCurrencyQueryCompiler extends QueryCompiler(ComposedSchema) {
  import CountryData._

  val selectElaborator =  new SelectElaborator(Map(
    ComposedSchema.tpe("Query").dealias -> {
      case Select("currency", List(StringBinding("code", code)), child) =>
        Wrap("currency", Unique(FieldEquals("code", code), child)).rightIor
      case Select("country", List(StringBinding("code", code)), child) =>
        Wrap("country", Unique(FieldEquals("code", code), child)).rightIor
      case Select("countries", _, child) =>
        Wrap("countries", child).rightIor
    }
  ))

  val countryCurrencyJoin = (c: Cursor, q: Query) =>
    c.focus match {
      case c: Country =>
        Wrap("currency", Unique(FieldEquals("code", c.currencyCode), q)).rightIor
      case _ =>
        mkErrorResult(s"Unexpected cursor focus type in countryCurrencyJoin")
    }

  val componentElaborator = ComponentElaborator(
    Mapping(ComposedSchema.tpe("Query"), "country", "CountrySchema"),
    Mapping(ComposedSchema.tpe("Query"), "countries", "CountrySchema"),
    Mapping(ComposedSchema.tpe("Country"), "currency", "CurrencySchema", countryCurrencyJoin)
  )

  val phases = List(selectElaborator, componentElaborator)
}

object CountryCurrencyQueryInterpreter extends
  ComposedQueryInterpreter[Id](Map(
    "CountrySchema"  -> CountryQueryInterpreter,
    "CurrencySchema" -> CurrencyQueryInterpreter
  ))

object CurrencyData {
  case class Currency(
    code: String,
    exchangeRate: Double
  )

  val EUR = Currency("EUR", 1.12)
  val GBP = Currency("GBP", 1.25)

  val currencies = List(EUR, GBP)
}

import CurrencyData.{ Currency, currencies }

object CurrencyQueryInterpreter extends DataTypeQueryInterpreter[Id](
  {
    case "currency" => (ListType(CurrencySchema.tpe("Currency")), currencies)
  },
  {
    case (c: Currency, "code")         => c.code
    case (c: Currency, "exchangeRate") => c.exchangeRate
  }
)

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

import CountryData.{ Country, countries }

object CountryQueryInterpreter extends DataTypeQueryInterpreter[Id](
  {
    case "country" | "countries"  => (ListType(CountrySchema.tpe("Country")), countries)
  },
  {
    case (c: Country, "code") => c.code
    case (c: Country, "name") => c.name
  }
)
