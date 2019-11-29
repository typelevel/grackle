// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Id
import cats.implicits._

import edu.gemini.grackle._
import Query._, Binding._, Predicate._
import QueryCompiler._, ComponentElaborator.Mapping
import QueryInterpreter.mkErrorResult

/* Currency component */

object CurrencyData {
  case class Currency(
    code: String,
    exchangeRate: Double
  )

  val EUR = Currency("EUR", 1.12)
  val GBP = Currency("GBP", 1.25)

  val currencies = List(EUR, GBP)

  val CurrencyType = CurrencySchema.tpe("Currency")
}

import CurrencyData.{ Currency, CurrencyType, currencies }

object CurrencyQueryInterpreter extends DataTypeQueryInterpreter[Id](
  {
    case "currency" => (ListType(CurrencyType), currencies)
  },
  {
    case (c: Currency, "code")         => c.code
    case (c: Currency, "exchangeRate") => c.exchangeRate
  }
)

object CurrencyQueryCompiler extends QueryCompiler(CurrencySchema) {
  val QueryType = CurrencySchema.tpe("Query").dealias

  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("currency", List(StringBinding("code", code)), child) =>
        Wrap("currency", Unique(FieldEquals("code", code), child)).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

/* Country component */

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

  val CountryType = CountrySchema.tpe("Country")
}

import CountryData.{ Country, CountryType, countries }

object CountryQueryInterpreter extends DataTypeQueryInterpreter[Id](
  {
    case "country" | "countries"  => (ListType(CountryType), countries)
  },
  {
    case (c: Country, "code") => c.code
    case (c: Country, "name") => c.name
  }
)

object CountryQueryCompiler extends QueryCompiler(CountrySchema) {
  val QueryType = CountrySchema.tpe("Query").dealias

  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(StringBinding("code", code)), child) =>
        Wrap("country", Unique(FieldEquals("code", code), child)).rightIor
      case Select("countries", _, child) =>
        Wrap("countries", child).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

/* Composition */

object ComposedQueryCompiler extends QueryCompiler(ComposedSchema) {
  import CountryData._

  val QueryType = ComposedSchema.tpe("Query").dealias
  val CountryType = ComposedSchema.tpe("Country")

  val selectElaborator =  new SelectElaborator(Map(
    QueryType -> {
      case Select("currency", List(StringBinding("code", code)), child) =>
        Wrap("currency", Unique(FieldEquals("code", code), child)).rightIor
      case Select("country", List(StringBinding("code", code)), child) =>
        Wrap("country", Unique(FieldEquals("code", code), child)).rightIor
      case Select("countries", _, child) =>
        Wrap("countries", child).rightIor
    }
  ))

  val countryCurrencyJoin = (c: Cursor, q: Query) =>
    (c.focus, q) match {
      case (c: Country, Select("currency", _, child)) =>
        Wrap("currency", Unique(FieldEquals("code", c.currencyCode), child)).rightIor
      case _ =>
        mkErrorResult(s"Unexpected cursor focus type in countryCurrencyJoin")
    }

  val componentElaborator = ComponentElaborator(
    Mapping(QueryType, "country", "CountryComponent"),
    Mapping(QueryType, "countries", "CountryComponent"),
    Mapping(CountryType, "currency", "CurrencyComponent", countryCurrencyJoin)
  )

  val phases = List(componentElaborator, selectElaborator)
}

object ComposedQueryInterpreter extends
  ComposedQueryInterpreter[Id](Map(
    "CountryComponent"  -> CountryQueryInterpreter,
    "CurrencyComponent" -> CurrencyQueryInterpreter
  ))
