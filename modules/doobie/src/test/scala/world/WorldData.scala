// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.Bracket
import cats.implicits._
import doobie.Transactor
import io.chrisdavenport.log4cats.Logger

import edu.gemini.grackle._, doobie._
import Query._, Binding._, Predicate._
import QueryCompiler._
import QueryInterpreter.mkErrorResult
import DoobiePredicate._
import DoobieMapping._, FieldMapping._
import ScalarType._

import WorldSchema._

object WorldData extends DoobieMapping {
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
            List(Join(ColumnRef("country", "code", StringType), ColumnRef("city", "countrycode", StringType))),
            countryCityJoin
          ),
          "languages" -> Subobject(ListType(LanguageType),
            List(Join(ColumnRef("country", "code", StringType), ColumnRef("countryLanguage", "countrycode", StringType))),
            countryLanguageJoin
          )
        )
    )

  def countryCityJoin(c: Cursor, q: Query): Result[Query] = q match {
    case Select("cities", Nil, child) =>
      val code = c.attribute("code").right.get.asInstanceOf[String]
      Wrap("cities", Filter(AttrEquals("countrycode", code), child)).rightIor
    case _ => mkErrorResult("Bad staging join")
  }

  def countryLanguageJoin(c: Cursor, q: Query): Result[Query] = q match {
    case Select("languages", Nil, child) =>
      val code = c.attribute("code").right.get.asInstanceOf[String]
      Wrap("languages", Filter(AttrContains(List("countries", "code"), code), child)).rightIor
    case _ => mkErrorResult("Bad staging join")
  }

  val cityMapping =
    ObjectMapping(
      tpe = CityType,
      key = List(
        ColumnRef("city", "id", IntType),
        ColumnRef("city", "countrycode", IntType)
      ),
      fieldMappings =
        List(
          "name" -> ColumnRef("city", "name", StringType),
          "country" -> Subobject(CountryType,
            List(Join(ColumnRef("city", "countrycode", StringType), ColumnRef("country", "code", StringType))),
            cityCountryJoin
          ),
          "district" -> ColumnRef("city", "district", StringType),
          "population" -> ColumnRef("city", "population", IntType)
        )
    )

  def cityCountryJoin(c: Cursor, q: Query): Result[Query] = q match {
    case Select("country", Nil, child) =>
      val countrycode = c.attribute("countrycode").right.get.asInstanceOf[String]
      Wrap("country", Unique(AttrEquals("code", countrycode), child)).rightIor
    case _ => mkErrorResult("Bad staging join")
  }

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
            List(Join(ColumnRef("countryLanguage", "countrycode", StringType), ColumnRef("country", "code", StringType))),
            languageCountryJoin
          )
        )
    )

  def languageCountryJoin(c: Cursor, q: Query): Result[Query] = q match {
    case Select("countries", Nil, child) =>
      val language = c.attribute("language").right.get.asInstanceOf[String]
      Wrap("countries", Filter(FieldContains(List("languages", "language"), language), child)).rightIor
    case _ => mkErrorResult("Bad staging join")
  }

  val objectMappings = List(queryMapping, countryMapping, cityMapping, languageMapping)
}

object WorldQueryCompiler extends QueryCompiler(WorldSchema) {
  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("countries", Nil, child) =>
        Wrap("countries", child).rightIor
      case Select("country", List(StringBinding("code", code)), child) =>
        Wrap("country", Unique(AttrEquals("code", code), child)).rightIor
      case Select("cities", List(StringBinding("namePattern", namePattern)), child) =>
        Wrap("cities", Filter(FieldLike("name", namePattern, true), child)).rightIor
      }
  ))

  val stagingElaborator = new StagingElaborator(WorldData)

  val phases = List(selectElaborator, stagingElaborator)
}

object WorldQueryInterpreter {
  def fromTransactor[F[_]](xa: Transactor[F])
    (implicit brkt: Bracket[F, Throwable], logger: Logger[F]): DoobieQueryInterpreter[F] =
      new DoobieQueryInterpreter[F](WorldSchema, WorldData, xa, logger)
}
