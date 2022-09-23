// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.Monad
import cats.data.Ior
import cats.implicits._
import edu.gemini.grackle._
import edu.gemini.grackle.sql.Like
import edu.gemini.grackle.syntax._

import Query._
import Predicate._
import Value._
import QueryCompiler._
import QueryInterpreter.mkErrorResult

/* Currency component */

object CurrencyData {
  case class Currency(
    code: String,
    exchangeRate: Double,
    countryCode: String
  )

  val BRL = Currency("BRL", 0.25, "BRA")
  val EUR = Currency("EUR", 1.12, "NLD")
  val GBP = Currency("GBP", 1.25, "GBR")

  val currencies = List(BRL, EUR, GBP)

}

class CurrencyMapping[F[_] : Monad] extends ValueMapping[F] {
  import CurrencyData._

  val schema =
    schema"""
      type Query {
        allCurrencies: [Currency!]!
      }
      type Currency {
        code: String!
        exchangeRate: Float!
        countryCode: String!
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
            ValueRoot("allCurrencies", currencies)
          )
      ),
      ValueObjectMapping[Currency](
        tpe = CurrencyType,
        fieldMappings =
          List(
            ValueField("code", _.code),
            ValueField("exchangeRate", _.exchangeRate),
            ValueField("countryCode", _.countryCode)
          )
      )
  )
}

object CurrencyMapping {
  def apply[F[_] : Monad]: CurrencyMapping[F] = new CurrencyMapping[F]
}

/* Composition */

class SqlComposedMapping[F[_] : Monad]
  (world: Mapping[F], currency: Mapping[F]) extends Mapping[F] {
  val schema =
    schema"""
      type Query {
        cities(namePattern: String = "%"): [City!]
        country(code: String): Country
        countries: [Country!]
        currencies: [Currency!]!
      }
      type City {
        name: String!
        country: Country!
        district: String!
        population: Int!
      }
      type Language {
        language: String!
        isOfficial: Boolean!
        percentage: Float!
        countries: [Country!]!
      }
      type Currency {
        code: String!
        exchangeRate: Float!
        countryCode: String!
      }
      type Country {
        name: String!
        continent: String!
        region: String!
        surfacearea: Float!
        indepyear: Int
        population: Int!
        lifeexpectancy: Float
        gnp: String
        gnpold: String
        localname: String!
        governmentform: String!
        headofstate: String
        capitalId: Int
        code2: String!
        cities: [City!]!
        languages: [Language!]!
        currencies: [Currency!]!
      }
    """

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")
  val CurrencyType = schema.ref("Currency")
  val CityType = schema.ref("City")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            Delegate("country", world),
            Delegate("countries", world),
            Delegate("cities", world)
          )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings =
          List(
            Delegate("currencies", currency, countryCurrencyJoin)
          )
      )
    )

  def countryCurrencyJoin(c: Cursor, q: Query): Result[Query] =
    (c.fieldAs[String]("code"), q) match {
      case (Ior.Right(countryCode: String), Select("currencies", _, child)) =>
        Select("allCurrencies", Nil, Filter(Eql(CurrencyType / "countryCode", Const(countryCode)), child)).rightIor
      case _ => mkErrorResult(s"Expected 'code' attribute at ${c.tpe}")
    }

  override val selectElaborator =  new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(Binding("code", StringValue(code))), child) =>
        Select("country", Nil, Unique(Filter(Eql(CountryType / "code", Const(code)), child))).rightIor
      case Select("countries", _, child) =>
        Select("countries", Nil, child).rightIor
      case Select("cities", List(Binding("namePattern", StringValue(namePattern))), child) =>
        Select("cities", Nil, Filter(Like(CityType / "name", namePattern, true), child)).rightIor
    }
  ))
}
