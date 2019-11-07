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

import DoobiePredicate._
import Predicate._
import Query._, Binding._
import QueryInterpreter.{ mkErrorResult, ProtoJson }

class WorldCurrencyQueryInterpreter[F[_]](xa: Transactor[F])(implicit brkt: Bracket[F, Throwable], logger: Logger[F])
  extends ComposedQueryInterpreter[F] {
  val schema = ComposedWorldSchema
  import schema._

  val currencyMapping =
    ObjectMapping(
      tpe = CurrencyType,
      interpreter = new CurrencyQueryInterpreter[F],
      fieldMappings = Nil
    )

  val countryMapping =
    ObjectMapping(
      tpe = CountryType,
      interpreter = ComposedWorldQueryInterpreter.fromTransactor(xa),
      fieldMappings =
        List(
          "currency" -> Subobject(currencyMapping, countryCurrencyJoin)
        )
    )

  val queryMapping =
    ObjectMapping(
      tpe = QueryType,
      interpreter = this,
      fieldMappings =
        List(
          "country" -> Subobject(countryMapping),
          "cities" -> Subobject(countryMapping),
          "countries" -> Subobject(countryMapping),
          "currency" -> Subobject(currencyMapping)
        )
    )

  def countryCurrencyJoin(c: Cursor, q: Query): Result[Query] = {
    c.attribute("code") match {
      case Ior.Right(countryCode: String) =>
        Select("currency", List(StringBinding("countryCode", countryCode)), q).rightIor
      case _ =>
        mkErrorResult("Bad query")
    }
  }

  val objectMappings = List(queryMapping, countryMapping, currencyMapping)
}

object WorldCurrencyQueryInterpreter {
  def fromTransactor[F[_]](xa: Transactor[F])
    (implicit brkt: Bracket[F, Throwable], logger: Logger[F]): WorldCurrencyQueryInterpreter[F] = {
      new WorldCurrencyQueryInterpreter[F](xa)
    }
}

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

class CurrencyQueryInterpreter[F[_]](override implicit val F: Monad[F]) extends QueryInterpreter[F] {
  val schema = ComposedWorldSchema
  import schema._

  import CurrencyData._

  def runRootValue(query: Query): F[Result[ProtoJson]] = {
    query match {
      case Select("currency", List(StringBinding("countryCode", countryCode)), child) =>
        runValue(Unique(FieldEquals("countryCode", countryCode), child), NullableType(CurrencyType), CurrencyCursor(currencies)).pure[F]
      case _ => mkErrorResult("Bad query").pure[F]
    }
  }
}

case class CurrencyCursor(focus: Any) extends DataTypeCursor {
  import CurrencyData._

  def mkCursor(focus: Any): Cursor = CurrencyCursor(focus)

  def hasField(fieldName: String): Boolean = (focus, fieldName) match {
    case (_: Currency, "code" | "exchangeRate" | "countryCode") => true
    case _ => false
  }

  def field(fieldName: String, args: Map[String, Any]): Result[Cursor] = (focus, fieldName) match {
    case (c: Currency, "code") => mkCursor(c.code).rightIor
    case (c: Currency, "exchangeRate") => mkCursor(c.exchangeRate).rightIor
    case (c: Currency, "countryCode") => mkCursor(c.countryCode).rightIor
    case _ => mkErrorResult(s"No field '$fieldName'")
  }
}

object ComposedWorldData extends DoobieMapping {
  import DoobieMapping._, FieldMapping._
  import ComposedWorldSchema._
  import ScalarType._

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

trait ComposedWorldQueryInterpreter[F[_]] extends DoobieQueryInterpreter[F] {
  val schema = ComposedWorldSchema

  def prepare(query: Query): Query = query match {
    case Select("country", args@List(StringBinding("code", code)), child) =>
      (Select("country", args, Unique(AttrEquals("code", code), child)))
    case Select("cities", args@List(StringBinding("namePattern", namePattern)), child) =>
      Select("cities", args, Filter(FieldLike("name", namePattern, true), child))
    case _ =>
      query
  }
}

object ComposedWorldQueryInterpreter {
  def fromTransactor[F[_]](xa0: Transactor[F])
    (implicit brkt: Bracket[F, Throwable], logger0: Logger[F]): ComposedWorldQueryInterpreter[F] =
      new ComposedWorldQueryInterpreter[F] {
        val mapping = ComposedWorldData
        val xa = xa0
        val logger = logger0
      }
}
