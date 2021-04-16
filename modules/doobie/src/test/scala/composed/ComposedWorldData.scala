// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import _root_.doobie.util.meta.Meta
import _root_.doobie.util.transactor.Transactor
import cats.Monad
import cats.data.Ior
import cats.effect.Sync
import cats.implicits._
import edu.gemini.grackle._
import edu.gemini.grackle.doobie._
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

/* World component */

trait WorldMapping[F[_]] extends DoobieMapping[F] {

  val schema =
    schema"""
      type Query {
        cities(namePattern: String = "%"): [City!]
        country(code: String): Country
        countries: [Country!]
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
      }
    """

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")
  val CityType = schema.ref("City")
  val LanguageType = schema.ref("Language")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlRoot("cities"),
            SqlRoot("country"),
            SqlRoot("countries")
          )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings =
          List(
            SqlAttribute("code", ColumnRef("country", "code", Meta[String]), key = true),
            SqlField("name", ColumnRef("country", "name", Meta[String])),
            SqlField("continent", ColumnRef("country", "continent", Meta[String])),
            SqlField("region", ColumnRef("country", "region", Meta[String])),
            SqlField("surfacearea", ColumnRef("country", "surfacearea", Meta[Float])),
            SqlField("indepyear", ColumnRef("country", "indepyear", Meta[Short])),
            SqlField("population", ColumnRef("country", "population", Meta[Int])),
            SqlField("lifeexpectancy", ColumnRef("country", "lifeexpectancy", Meta[Float])),
            SqlField("gnp", ColumnRef("country", "gnp", Meta[BigDecimal])),
            SqlField("gnpold", ColumnRef("country", "gnpold", Meta[BigDecimal])),
            SqlField("localname", ColumnRef("country", "localname", Meta[String])),
            SqlField("governmentform", ColumnRef("country", "governmentform", Meta[String])),
            SqlField("headofstate", ColumnRef("country", "headofstate", Meta[String])),
            SqlField("capitalId", ColumnRef("country", "capitalId", Meta[Int])),
            SqlField("code2", ColumnRef("country", "code2", Meta[String])),
            SqlObject("cities", Join(ColumnRef("country", "code", Meta[String]), ColumnRef("city", "countrycode", Meta[String]))),
            SqlObject("languages", Join(ColumnRef("country", "code", Meta[String]), ColumnRef("countryLanguage", "countrycode", Meta[String])))
          )
      ),
      ObjectMapping(
        tpe = CityType,
        fieldMappings =
          List(
            SqlAttribute("id", ColumnRef("city", "id", Meta[Int]), key = true),
            SqlAttribute("countrycode", ColumnRef("city", "countrycode", Meta[String])),
            SqlField("name", ColumnRef("city", "name", Meta[String])),
            SqlObject("country", Join(ColumnRef("city", "countrycode", Meta[String]), ColumnRef("country", "code", Meta[String]))),
            SqlField("district", ColumnRef("city", "district", Meta[String])),
            SqlField("population", ColumnRef("city", "population", Meta[Int]))
          )
      ),
      ObjectMapping(
        tpe = LanguageType,
        fieldMappings =
          List(
            SqlField("language", ColumnRef("countryLanguage", "language", Meta[String]), key = true),
            SqlField("isOfficial", ColumnRef("countryLanguage", "isOfficial", Meta[Boolean])),
            SqlField("percentage", ColumnRef("countryLanguage", "percentage", Meta[Float])),
            SqlAttribute("countrycode", ColumnRef("countryLanguage", "countrycode", Meta[String])),
            SqlObject("countries", Join(ColumnRef("countryLanguage", "countrycode", Meta[String]), ColumnRef("country", "code", Meta[String])))
          )
      )
    )
}

object WorldMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with WorldMapping[F]

}

/* Composition */

class ComposedMapping[F[_] : Monad]
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
    (c.attribute("code"), q) match {
      case (Ior.Right(countryCode: String), Select("currencies", _, child)) =>
        Select("allCurrencies", Nil, Filter(Eql(FieldPath(List("countryCode")), Const(countryCode)), child)).rightIor
      case _ => mkErrorResult(s"Expected 'code' attribute at ${c.tpe}")
    }

  override val selectElaborator =  new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(Binding("code", StringValue(code))), child) =>
        Select("country", Nil, Unique(Eql(AttrPath(List("code")), Const(code)), child)).rightIor
      case Select("countries", _, child) =>
        Select("countries", Nil, child).rightIor
      case Select("cities", List(Binding("namePattern", StringValue(namePattern))), child) =>
        Select("cities", Nil, Filter(Like(FieldPath(List("name")), namePattern, true), child)).rightIor
    }
  ))
}

object ComposedMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new ComposedMapping[F](WorldMapping.mkMapping(transactor, monitor), CurrencyMapping[F])
}
