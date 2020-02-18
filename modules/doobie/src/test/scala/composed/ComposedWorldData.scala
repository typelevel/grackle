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

import Query._, Predicate._, Value._
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

  val CurrencyType = CurrencySchema.ref("Currency")

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

  val countryMapping =
    ObjectMapping(
      tpe = "Country",
      key = List(ColumnRef("country", "code")),
      fieldMappings =
        List(
          "name" -> ColumnRef("country", "name"),
          "continent" -> ColumnRef("country", "continent"),
          "region" -> ColumnRef("country", "region"),
          "surfacearea" -> ColumnRef("country", "surfacearea"),
          "indepyear" -> ColumnRef("country", "indepyear"),
          "population" -> ColumnRef("country", "population"),
          "lifeexpectancy" -> ColumnRef("country", "lifeexpectancy"),
          "gnp" -> ColumnRef("country", "gnp"),
          "gnpold" -> ColumnRef("country", "gnpold"),
          "localname" -> ColumnRef("country", "localname"),
          "governmentform" -> ColumnRef("country", "governmentform"),
          "headofstate" -> ColumnRef("country", "headofstate"),
          "capitalId" -> ColumnRef("country", "capitalId"),
          "code2" -> ColumnRef("country", "code2"),
          "cities" -> Subobject(
            List(Join(ColumnRef("country", "code"), ColumnRef("city", "countrycode")))),
          "languages" -> Subobject(
            List(Join(ColumnRef("country", "code"), ColumnRef("countryLanguage", "countrycode"))))
        ),
      attributeMappings =
        List(
          "code" -> Attr[String](ColumnRef("country", "code"))
        )
    )

  val cityMapping =
    ObjectMapping(
      tpe = "City",
      key = List(ColumnRef("city", "id")),
      fieldMappings =
        List(
          "name" -> ColumnRef("city", "name"),
          "country" -> Subobject(
            List(Join(ColumnRef("city", "countrycode"), ColumnRef("country", "code")))),
          "district" -> ColumnRef("city", "district"),
          "population" -> ColumnRef("city", "population")
        ),
      attributeMappings =
        List(
          "id" -> Attr[Int](ColumnRef("city", "id")),
          "countrycode" -> Attr[String](ColumnRef("city", "countrycode")),
        )
    )

  val languageMapping =
    ObjectMapping(
      tpe = "Language",
      key = List(ColumnRef("countryLanguage", "language")),
      fieldMappings =
        List(
          "language" -> ColumnRef("countryLanguage", "language"),
          "isOfficial" -> ColumnRef("countryLanguage", "isOfficial"),
          "percentage" -> ColumnRef("countryLanguage", "percentage"),
          "countries" -> Subobject(
            List(Join(ColumnRef("countryLanguage", "countrycode"), ColumnRef("country", "code"))))
        ),
      attributeMappings =
        List(
          "countrycode" -> Attr[String](ColumnRef("countryLanguage", "countrycode"))
        )
    )

  val objectMappings = List(countryMapping, cityMapping, languageMapping)
}

object WorldQueryInterpreter {
  def fromTransactor[F[_]](xa: Transactor[F])
    (implicit brkt: Bracket[F, Throwable], logger: Logger[F]): DoobieQueryInterpreter[F] =
      new DoobieQueryInterpreter[F](WorldData, xa, logger)
}

/* Composition */

object ComposedQueryCompiler extends QueryCompiler(ComposedSchema) {
  val QueryType = ComposedSchema.ref("Query")
  val CountryType = ComposedSchema.ref("Country")

  val selectElaborator =  new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(Binding("code", StringValue(code))), child) =>
        Select("country", Nil, Unique(AttrEquals("code", code), child)).rightIor
      case Select("countries", _, child) =>
        Select("countries", Nil, child).rightIor
      case Select("cities", List(Binding("namePattern", StringValue(namePattern))), child) =>
        Select("cities", Nil, Filter(FieldLike("name", namePattern, true), child)).rightIor
    }
  ))

  val countryCurrencyJoin = (c: Cursor, q: Query) =>
    (c.attribute("code"), q) match {
      case (Ior.Right(countryCode: String), Select("currencies", _, child)) =>
        Select("currencies", Nil, Filter(FieldEquals("countryCode", countryCode), child)).rightIor
      case _ => mkErrorResult(s"Expected 'code' attribute at ${c.tpe}")
    }

  val componentElaborator = ComponentElaborator(
    Mapping(QueryType, "country", "WorldComponent"),
    Mapping(QueryType, "countries", "WorldComponent"),
    Mapping(QueryType, "cities", "WorldComponent"),
    Mapping(CountryType, "currencies", "CurrencyComponent", countryCurrencyJoin)
  )

  val phases = List(componentElaborator, selectElaborator)
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
