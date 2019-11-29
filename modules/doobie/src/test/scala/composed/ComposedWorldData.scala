// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Monad
import cats.data.Ior
import cats.effect.Bracket
import cats.implicits._
import doobie.Transactor
import edu.gemini.grackle._, doobie._
import io.chrisdavenport.log4cats.Logger

import Query._, Binding._, Predicate._
import QueryCompiler._, ComponentElaborator.Mapping
import QueryInterpreter.mkErrorResult
import DoobiePredicate._

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

object CurrencyQueryInterpreter {
  import CurrencyData._

  val CurrencyType = CurrencySchema.tpe("Currency")

  def apply[F[_]: Monad] = new DataTypeQueryInterpreter[F](
    {
      case "currencies" => (ListType(CurrencyType), currencies)
    },
    {
      case (c: Currency, "code")         => c.code
      case (c: Currency, "exchangeRate") => c.exchangeRate
      case (c: Currency, "countryCode")  => c.countryCode
    }
  )
}

/* World component */

object WorldData extends DoobieMapping {
  import DoobieMapping._, FieldMapping._
  import ScalarType._

  val QueryType = WorldSchema.tpe("Query")
  val CountryType = WorldSchema.tpe("Country")
  val CityType = WorldSchema.tpe("City")
  val LanguageType = WorldSchema.tpe("Language")

  val queryMapping =
    ObjectMapping(
      tpe = QueryType,
      key = Nil,
      fieldMappings =
        List(
          "cities" -> Subobject(ListType(CityType), Nil),
          "country" -> Subobject(CountryType, Nil),
          "countries" -> Subobject(ListType(CountryType), Nil)
        )
    )

  val countryMapping =
    ObjectMapping(
      tpe = CountryType,
      key = List(ColumnRef("country", "code", StringType)),
      fieldMappings =
        List(
          "name" -> ColumnRef("country", "name", StringType),
          "continent" -> ColumnRef("country", "continent", StringType),
          "region" -> ColumnRef("country", "region", StringType),
          "surfacearea" -> ColumnRef("country", "surfacearea", FloatType),
          "indepyear" -> ColumnRef("country", "indepyear", IntType),
          "population" -> ColumnRef("country", "population", IntType),
          "lifeexpectancy" -> ColumnRef("country", "lifeexpectancy", FloatType),
          "gnp" -> ColumnRef("country", "gnp", StringType),
          "gnpold" -> ColumnRef("country", "gnpold", StringType),
          "localname" -> ColumnRef("country", "localname", StringType),
          "governmentform" -> ColumnRef("country", "governmentform", StringType),
          "headofstate" -> ColumnRef("country", "headofstate", StringType),
          "capitalId" -> ColumnRef("country", "capitalId", IntType),
          "code2" -> ColumnRef("country", "code2", StringType),
          "cities" -> Subobject(ListType(CityType),
            List(Join(ColumnRef("country", "code", StringType), ColumnRef("city", "countrycode", StringType)))),
          "languages" -> Subobject(ListType(LanguageType),
            List(Join(ColumnRef("country", "code", StringType), ColumnRef("countryLanguage", "countrycode", StringType))))
        )
    )

  val cityMapping =
    ObjectMapping(
      tpe = CityType,
      key = List(ColumnRef("city", "id", IntType)),
      fieldMappings =
        List(
          "name" -> ColumnRef("city", "name", StringType),
          "country" -> Subobject(CountryType,
            List(Join(ColumnRef("city", "countrycode", StringType), ColumnRef("country", "code", StringType)))),
          "district" -> ColumnRef("city", "district", StringType),
          "population" -> ColumnRef("city", "population", IntType)
        )
    )

  val languageMapping =
    ObjectMapping(
      tpe = LanguageType,
      key = List(ColumnRef("countryLanguage", "language", StringType)),
      fieldMappings =
        List(
          "language" -> ColumnRef("countryLanguage", "language", StringType),
          "isOfficial" -> ColumnRef("countryLanguage", "isOfficial", BooleanType),
          "percentage" -> ColumnRef("countryLanguage", "percentage", FloatType),
          "countries" -> Subobject(ListType(CountryType),
            List(Join(ColumnRef("countryLanguage", "countrycode", StringType), ColumnRef("country", "code", StringType))))
        )
    )

  val objectMappings = List(queryMapping, countryMapping, cityMapping, languageMapping)
}

object WorldQueryInterpreter {
  def fromTransactor[F[_]](xa: Transactor[F])
    (implicit brkt: Bracket[F, Throwable], logger: Logger[F]): DoobieQueryInterpreter[F] =
      new DoobieQueryInterpreter[F](WorldData, xa, logger)
}

/* Composition */

object ComposedQueryCompiler extends QueryCompiler(ComposedSchema) {
  val QueryType = ComposedSchema.tpe("Query")
  val CountryType = ComposedSchema.tpe("Country")

  val selectElaborator =  new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(StringBinding("code", code)), child) =>
        Wrap("country", Unique(AttrEquals("code", code), child)).rightIor
      case Select("countries", _, child) =>
        Wrap("countries", child).rightIor
      case Select("cities", List(StringBinding("namePattern", namePattern)), child) =>
        Wrap("cities", Filter(FieldLike("name", namePattern, true), child)).rightIor
    }
  ))

  val countryCurrencyJoin = (c: Cursor, q: Query) =>
    c.attribute("code") match {
      case Ior.Right(countryCode: String) =>
        Wrap("currencies", Filter(FieldEquals("countryCode", countryCode), q)).rightIor
      case _ => mkErrorResult(s"Expected 'code' attribute at ${c.tpe.shortString}")
    }

  val componentElaborator = ComponentElaborator(
    Mapping(QueryType, "country", "WorldComponent"),
    Mapping(QueryType, "countries", "WorldComponent"),
    Mapping(QueryType, "cities", "WorldComponent"),
    Mapping(CountryType, "currencies", "CurrencyComponent", countryCurrencyJoin)
  )

  val phases = List(selectElaborator, componentElaborator)
}

object ComposedQueryInterpreter {
  def fromTransactor[F[_]](xa: Transactor[F])
    (implicit brkt: Bracket[F, Throwable], logger0: Logger[F]): ComposedQueryInterpreter[F] = {
      val mapping: Map[String, QueryInterpreter[F]] = Map(
        "WorldComponent"    -> WorldQueryInterpreter.fromTransactor(xa),
        "CurrencyComponent" -> CurrencyQueryInterpreter[F]
      )
      new ComposedQueryInterpreter(mapping)
  }
}
