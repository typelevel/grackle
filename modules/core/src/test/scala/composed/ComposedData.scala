// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Id
import cats.implicits._

import edu.gemini.grackle._
import Query._, Binding._, Predicate._
import QueryCompiler._, ComponentElaborator.Mapping
import QueryInterpreter.mkErrorResult

import ComposedSchema._

object CurrencyQueryCompiler extends QueryCompiler(ComposedSchema) {
  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("currency", List(StringBinding("code", code)), child) =>
        Wrap("currency", Unique(FieldEquals("code", code), child)).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

object CountryQueryCompiler extends QueryCompiler(ComposedSchema) {
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

object CountryCurrencyQueryCompiler extends QueryCompiler(ComposedSchema) {
  import CountryData._

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
    c.focus match {
      case c: Country =>
        Wrap("currency", Unique(FieldEquals("code", c.currencyCode), q)).rightIor
      case _ =>
        mkErrorResult(s"Unexpected cursor focus type in countryCurrencyJoin")
    }

  val componentElaborator = ComponentElaborator(
    Mapping(QueryType, "country", CountrySchema),
    Mapping(QueryType, "countries", CountrySchema),
    Mapping(CountryType, "currency", CurrencySchema, countryCurrencyJoin)
  )

  val phases = List(selectElaborator, componentElaborator)
}

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
      case _ => mkErrorResult(s"Unexpected query in CurrencyQueryInterpreter rootCursor: ${query.render}")
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
      case _ =>
        Thread.dumpStack
        mkErrorResult(s"Unexpected query in CountryQueryInterpreter rootCursor: $query")
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
