// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Id
import cats.implicits._

import edu.gemini.grackle._
import Query._, Predicate._, Value._
import QueryCompiler._, ComponentElaborator.Mapping
import QueryInterpreter.mkErrorResult

/* Currency component */

object CurrencyData {
  val schema =
    Schema(
      """
        type Query {
          fx(code: String): Currency
        }
        type Currency {
          code: String!
          exchangeRate: Float!
        }
      """
    ).right.get

  case class Currency(
    code: String,
    exchangeRate: Double
  )

  val EUR = Currency("EUR", 1.12)
  val GBP = Currency("GBP", 1.25)

  val currencies = List(EUR, GBP)

  val CurrencyType = schema.ref("Currency")
}

import CurrencyData.{ Currency, CurrencyType, currencies }

object CurrencyQueryInterpreter extends DataTypeQueryInterpreter[Id](
  {
    case "fx" => (ListType(CurrencyType), currencies)
  },
  {
    case (c: Currency, "code")         => c.code
    case (c: Currency, "exchangeRate") => c.exchangeRate
  }
)

object CurrencyQueryCompiler extends QueryCompiler(CurrencyData.schema) {
  val QueryType = CurrencyData.schema.ref("Query")

  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("fx", List(Binding("code", StringValue(code))), child) =>
        Select("fx", Nil, Unique(Eql(FieldPath(List("code")), Const(code)), child)).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

/* Country component */

object CountryData {
  val schema =
    Schema(
      """
        type Query {
          country(code: String): Country
          countries: [Country!]!
        }
        type Country {
          code: String!
          name: String!
        }
      """
    ).right.get

  case class Country(
    code: String,
    name: String,
    currencyCode: String
  )

  val DEU = Country("DEU", "Germany", "EUR")
  val FRA = Country("FRA", "France", "EUR")
  val GBR = Country("GBR", "United Kingdom", "GBP")

  val countries = List(DEU, FRA, GBR)

  val CountryType = schema.ref("Country")
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

object CountryQueryCompiler extends QueryCompiler(CountryData.schema) {
  val QueryType = CountryData.schema.ref("Query")

  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(Binding("code", StringValue(code))), child) =>
        Select("country", Nil, Unique(Eql(FieldPath(List("code")), Const(code)), child)).rightIor
      case Select("countries", _, child) =>
        Select("countries", Nil, child).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

/* Composition */

object ComposedData {
  val schema =
    Schema(
      """
        type Query {
          country(code: String): Country
          fx(code: String): Currency
          countries: [Country!]!
        }
        type Currency {
          code: String!
          exchangeRate: Float!
        }
        type Country {
          code: String!
          name: String!
          currency: Currency!
        }
      """
    ).right.get
}

object ComposedQueryCompiler extends QueryCompiler(ComposedData.schema) {
  import CountryData._

  val QueryType = ComposedData.schema.ref("Query")
  val CountryType = ComposedData.schema.ref("Country")

  val selectElaborator =  new SelectElaborator(Map(
    QueryType -> {
      case Select("fx", List(Binding("code", StringValue(code))), child) =>
        Select("fx", Nil, Unique(Eql(FieldPath(List("code")), Const(code)), child)).rightIor
      case Select("country", List(Binding("code", StringValue(code))), child) =>
        Select("country", Nil, Unique(Eql(FieldPath(List("code")), Const(code)), child)).rightIor
      case Select("countries", _, child) =>
        Select("countries", Nil, child).rightIor
    }
  ))

  val countryCurrencyJoin = (c: Cursor, q: Query) =>
    (c.focus, q) match {
      case (c: Country, Select("currency", _, child)) =>
        Select("fx", Nil, Unique(Eql(FieldPath(List("code")), Const(c.currencyCode)), child)).rightIor
      case _ =>
        mkErrorResult(s"Unexpected cursor focus type in countryCurrencyJoin")
    }

  val componentElaborator = ComponentElaborator(
    Mapping(QueryType, "country", "CountryComponent"),
    Mapping(QueryType, "countries", "CountryComponent"),
    Mapping(QueryType, "fx", "CurrencyComponent"),
    Mapping(CountryType, "currency", "CurrencyComponent", countryCurrencyJoin)
  )

  val phases = List(componentElaborator, selectElaborator)
}

object ComposedQueryInterpreter extends
  ComposedQueryInterpreter[Id](Map(
    "CountryComponent"  -> CountryQueryInterpreter,
    "CurrencyComponent" -> CurrencyQueryInterpreter
  ))
