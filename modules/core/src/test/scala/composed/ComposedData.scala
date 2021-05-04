// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Id
import cats.implicits._
import cats.catsInstancesForId

import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import Query._, Path._, Predicate._, Value._
import QueryCompiler._
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
}

object CurrencyMapping extends ValueMapping[Id] {
  import CurrencyData._

  val schema =
    schema"""
      type Query {
        fx(code: String): Currency
      }
      type Currency {
        code: String!
        exchangeRate: Float!
      }
    """

  val QueryType = schema.ref("Query")
  val CurrencyType = schema.ref("Currency")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            ValueRoot("fx", currencies)
          )
      ),
      ValueObjectMapping[Currency](
        tpe = CurrencyType,
        fieldMappings =
          List(
            ValueField("code", _.code),
            ValueField("exchangeRate", _.exchangeRate)
          )
      )
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("fx", List(Binding("code", StringValue(code))), child) =>
        Select("fx", Nil, Unique(Eql(UniquePath(List("code")), Const(code)), child)).rightIor
    }
  ))
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
}

object CountryMapping extends ValueMapping[Id] {
  import CountryData._

  val schema =
    schema"""
      type Query {
        country(code: String): Country
        countries: [Country!]!
      }
      type Country {
        code: String!
        name: String!
      }
    """

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            ValueRoot("country", countries),
            ValueRoot("countries", countries)
          )
      ),
      ValueObjectMapping[Country](
        tpe = CountryType,
        fieldMappings =
          List(
            ValueField("code", _.code),
            ValueField("name", _.name)
          )
      )
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(Binding("code", StringValue(code))), child) =>
        Select("country", Nil, Unique(Eql(UniquePath(List("code")), Const(code)), child)).rightIor
      case Select("countries", _, child) =>
        Select("countries", Nil, child).rightIor
    }
  ))
}

/* Composition */

object ComposedMapping extends Mapping[Id] {
  val schema =
    schema"""
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

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("fx", List(Binding("code", StringValue(code))), child) =>
        Select("fx", Nil, Unique(Eql(UniquePath(List("code")), Const(code)), child)).rightIor
      case Select("country", List(Binding("code", StringValue(code))), child) =>
        Select("country", Nil, Unique(Eql(UniquePath(List("code")), Const(code)), child)).rightIor
      case Select("countries", _, child) =>
        Select("countries", Nil, child).rightIor
    }
  ))

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            Delegate("country", CountryMapping),
            Delegate("countries", CountryMapping),
            Delegate("fx", CurrencyMapping)
          )
        ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings =
          List(
            Delegate("currency", CurrencyMapping, countryCurrencyJoin)
          )
      )
    )

  def countryCurrencyJoin(c: Cursor, q: Query): Result[Query] =
    (c.focus, q) match {
      case (c: CountryData.Country, Select("currency", _, child)) =>
        Select("fx", Nil, Unique(Eql(UniquePath(List("code")), Const(c.currencyCode)), child)).rightIor
      case _ =>
        mkErrorResult(s"Unexpected cursor focus type in countryCurrencyJoin")
    }

}
