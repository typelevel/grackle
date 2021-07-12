// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Monad
import cats.data.Ior
import cats.effect.{Resource, Sync}
import cats.implicits._
import skunk.Session

import edu.gemini.grackle._, skunk._, syntax._
import Query._, Path._, Predicate._, Value._
import QueryCompiler._
import QueryInterpreter.mkErrorResult
import sql.Like

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

trait WorldMapping[F[_]] extends SkunkMapping[F] {

  // ok nobody knows about codecs but us
  import _root_.skunk.codec.all._

  object country extends TableDef("country") {
    val code           = col("code", bpchar(3))
    val name           = col("name", text)
    val continent      = col("continent", varchar)
    val region         = col("region", varchar)
    val surfacearea    = col("surfacearea", varchar)
    val indepyear      = col("indepyear", int2.imap(_.toInt)(_.toShort).opt)
    val population     = col("population", int4)
    val lifeexpectancy = col("lifeexpectancy", varchar.opt)
    val gnp            = col("gnp", varchar.opt)
    val gnpold         = col("gnpold", varchar.opt)
    val localname      = col("localname", varchar)
    val governmentform = col("governmentform", varchar)
    val headofstate    = col("headofstate", varchar.opt)
    val capitalId      = col("capitalId", varchar.opt)
    val code2          = col("code2", varchar)
  }

  object city extends TableDef("city") {
    val id          = col("id", int4)
    val countrycode = col("countrycode", bpchar(3))
    val name        = col("name", text)
    val district    = col("district", varchar)
    val population  = col("population", int4)
  }

  object countryLanguage extends TableDef("countrylanguage") {
    val countrycode = col("countrycode", bpchar(3))
    val language = col("language", text)
    val isOfficial = col("isOfficial", varchar)
    val percentage = col("percentage", varchar)
  }

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
            SqlField("code", country.code, key = true, hidden = true),
            SqlField("name", country.name),
            SqlField("continent", country.continent),
            SqlField("region", country.region),
            SqlField("surfacearea", country.surfacearea),
            SqlField("indepyear", country.indepyear),
            SqlField("population", country.population),
            SqlField("lifeexpectancy", country.lifeexpectancy),
            SqlField("gnp", country.gnp),
            SqlField("gnpold", country.gnpold),
            SqlField("localname", country.localname),
            SqlField("governmentform", country.governmentform),
            SqlField("headofstate", country.headofstate),
            SqlField("capitalId", country.capitalId),
            SqlField("code2", country.code2),
            SqlObject("cities", Join(country.code, city.countrycode)),
            SqlObject("languages", Join(country.code, countryLanguage.countrycode))
          )
      ),
      ObjectMapping(
        tpe = CityType,
        fieldMappings =
          List(
            SqlField("id", city.id, key = true, hidden = true),
            SqlField("countrycode", city.countrycode, hidden = true),
            SqlField("name", city.name),
            SqlObject("country", Join(city.countrycode, country.code)),
            SqlField("district", city.district),
            SqlField("population", city.population)
          )
      ),
      ObjectMapping(
        tpe = LanguageType,
        fieldMappings =
          List(
            SqlField("language", countryLanguage.language, key = true),
            SqlField("isOfficial", countryLanguage.isOfficial),
            SqlField("percentage", countryLanguage.percentage),
            SqlField("countrycode", countryLanguage.countrycode, hidden = true),
            SqlObject("countries", Join(countryLanguage.countrycode, country.code))
          )
      )
    )
}

object WorldMapping extends SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with WorldMapping[F]

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
    (c.fieldAs[String]("code"), q) match {
      case (Ior.Right(countryCode: String), Select("currencies", _, child)) =>
        Select("allCurrencies", Nil, Filter(Eql(UniquePath(List("countryCode")), Const(countryCode)), child)).rightIor
      case _ => mkErrorResult(s"Expected 'code' attribute at ${c.tpe}")
    }

  override val selectElaborator =  new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(Binding("code", StringValue(code))), child) =>
        Select("country", Nil, Unique(Filter(Eql(UniquePath(List("code")), Const(code)), child))).rightIor
      case Select("countries", _, child) =>
        Select("countries", Nil, child).rightIor
      case Select("cities", List(Binding("namePattern", StringValue(namePattern))), child) =>
        Select("cities", Nil, Filter(Like(UniquePath(List("name")), namePattern, true), child)).rightIor
    }
  ))
}

object ComposedMapping extends SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new ComposedMapping[F](WorldMapping.mkMapping(pool), CurrencyMapping[F])
}
