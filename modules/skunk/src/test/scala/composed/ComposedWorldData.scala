// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Monad
import cats.data.Ior
import cats.effect.Sync
import cats.implicits._

import edu.gemini.grackle._, skunk._, syntax._
import edu.gemini.grackle.sql.Like
import Query._, Predicate._, Value._
import QueryCompiler._
import QueryInterpreter.mkErrorResult
import _root_.skunk.codec.all._
import cats.effect.Resource
import _root_.skunk.Session

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
            SqlAttribute("code", ColumnRef("country", "code", bpchar(3)), key = true),
            SqlField("name", ColumnRef("country", "name", text)),
            SqlField("continent", ColumnRef("country", "continent", text)),
            SqlField("region", ColumnRef("country", "region", text)),
            SqlField("surfacearea", ColumnRef("country", "surfacearea", float4)),
            SqlField("indepyear", ColumnRef("country", "indepyear", int2.opt)),
            SqlField("population", ColumnRef("country", "population", int4)),
            SqlField("lifeexpectancy", ColumnRef("country", "lifeexpectancy", float4.opt)),
            SqlField("gnp", ColumnRef("country", "gnp", numeric(10,2))),
            SqlField("gnpold", ColumnRef("country", "gnpold", numeric(10,2))),
            SqlField("localname", ColumnRef("country", "localname", text)),
            SqlField("governmentform", ColumnRef("country", "governmentform", text)),
            SqlField("headofstate", ColumnRef("country", "headofstate", text.opt)),
            SqlField("capitalId", ColumnRef("country", "capitalId", int4.opt)),
            SqlField("code2", ColumnRef("country", "code2", bpchar(2))),
            SqlObject("cities", Join(ColumnRef("country", "code", bpchar(3)), ColumnRef("city", "countrycode", bpchar(3)))),
            SqlObject("languages", Join(ColumnRef("country", "code", bpchar(3)), ColumnRef("countryLanguage", "countrycode", bpchar(3))))
          )
      ),
      ObjectMapping(
        tpe = CityType,
        fieldMappings =
          List(
            SqlAttribute("id", ColumnRef("city", "id", int4), key = true),
            SqlAttribute("countrycode", ColumnRef("city", "countrycode", bpchar(3))),
            SqlField("name", ColumnRef("city", "name", text)),
            SqlObject("country", Join(ColumnRef("city", "countrycode", bpchar(3)), ColumnRef("country", "code", bpchar(3)))),
            SqlField("district", ColumnRef("city", "district", text)),
            SqlField("population", ColumnRef("city", "population", int4))
          )
      ),
      ObjectMapping(
        tpe = LanguageType,
        fieldMappings =
          List(
            SqlField("language", ColumnRef("countryLanguage", "language", text), key = true),
            SqlField("isOfficial", ColumnRef("countryLanguage", "isOfficial", bool)),
            SqlField("percentage", ColumnRef("countryLanguage", "percentage", float4)),
            SqlAttribute("countrycode", ColumnRef("countryLanguage", "countrycode", bpchar(3))),
            SqlObject("countries", Join(ColumnRef("countryLanguage", "countrycode", bpchar(3)), ColumnRef("country", "code", bpchar(3))))
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

object ComposedMapping extends SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new ComposedMapping[F](WorldMapping.mkMapping(pool), CurrencyMapping[F])
}
