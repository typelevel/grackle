// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.Bracket
import doobie.Transactor
import edu.gemini.grackle._, doobie._
import io.chrisdavenport.log4cats.Logger

object WorldData extends DoobieMapping {
  import DoobieMapping._, FieldMapping._
  import WorldSchema._
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

abstract class WorldQueryInterpreter[F[_]](implicit val brkt: Bracket[F, Throwable])
  extends DoobieQueryInterpreter(WorldSchema)

object WorldQueryInterpreter {
  def fromTransactor[F[_]](xa0: Transactor[F])
    (implicit brkt: Bracket[F, Throwable], logger0: Logger[F]): WorldQueryInterpreter[F] =
      new WorldQueryInterpreter[F] {
        val mapping = WorldData
        val xa = xa0
        val logger = logger0
      }
}
