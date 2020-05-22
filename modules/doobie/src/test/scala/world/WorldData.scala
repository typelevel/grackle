// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.Bracket
import cats.implicits._
import doobie.Transactor
import io.chrisdavenport.log4cats.Logger

import edu.gemini.grackle._, doobie._
import Query._, Predicate._, Value._
import QueryCompiler._
import QueryInterpreter.mkErrorResult
import DoobiePredicate._
import DoobieMapping._, FieldMapping._

object WorldData extends DoobieMapping {
  val schema =
    Schema(
      """
        type Query {
          cities(namePattern: String = "%"): [City!]
          country(code: String): Country
          countries: [Country!]
          language(language: String): Language
          languages: [Language!]
          search(minPopulation: Int!, indepSince: Int!): [Country!]!
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
    ).right.get

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
            List(Join(ColumnRef("country", "code"), ColumnRef("city", "countrycode"))),
            countryCityJoin
          ),
          "languages" -> Subobject(
            List(Join(ColumnRef("country", "code"), ColumnRef("countryLanguage", "countrycode"))),
            countryLanguageJoin
          )
        ),
      attributeMappings =
        List(
          "code" -> Attr[String](ColumnRef("country", "code"))
        )
    )

  def countryCityJoin(c: Cursor, q: Query): Result[Query] = q match {
    case Select("cities", Nil, child) =>
      c.attribute("code").map { case (code: String) =>
        Select("cities", Nil, Filter(Eql(AttrPath(List("countrycode")), Const(code)), child))
      }
    case _ => mkErrorResult("Bad staging join")
  }

  def countryLanguageJoin(c: Cursor, q: Query): Result[Query] = q match {
    case Select("languages", Nil, child) =>
      c.attribute("code").map { case (code: String) =>
        Select("languages", Nil, Filter(Eql(AttrPath(List("countrycode")), Const(code)), child))
      }
    case _ => mkErrorResult("Bad staging join")
  }

  val cityMapping =
    ObjectMapping(
      tpe = "City",
      key = List(ColumnRef("city", "id")),
      fieldMappings =
        List(
          "name" -> ColumnRef("city", "name"),
          "country" -> Subobject(
            List(Join(ColumnRef("city", "countrycode"), ColumnRef("country", "code"))),
            cityCountryJoin
          ),
          "district" -> ColumnRef("city", "district"),
          "population" -> ColumnRef("city", "population")
        ),
      attributeMappings =
        List(
          "id" -> Attr[Int](ColumnRef("city", "id")),
          "countrycode" -> Attr[String](ColumnRef("city", "countrycode")),
        )
    )

  def cityCountryJoin(c: Cursor, q: Query): Result[Query] = q match {
    case Select("country", Nil, child) =>
      c.attribute("countrycode").map { case (countrycode: String) =>
        Select("country", Nil, Unique(Eql(AttrPath(List("code")), Const(countrycode)), child))
      }
    case _ => mkErrorResult("Bad staging join")
  }

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
            List(Join(ColumnRef("countryLanguage", "countrycode"), ColumnRef("country", "code"))),
            languageCountryJoin
          )
        ),
      attributeMappings =
        List(
          "countrycode" -> Attr[String](ColumnRef("countryLanguage", "countrycode"))
        )
    )

  def languageCountryJoin(c: Cursor, q: Query): Result[Query] = q match {
    case Select("countries", Nil, child) =>
      c.field("language").map { case ScalarFocus(language: String) =>
        Select("countries", Nil, Filter(Contains(FieldPath(List("languages", "language")), Const(language)), child))
      }
    case _ => mkErrorResult("Bad staging join")
  }

  val objectMappings = List(countryMapping, cityMapping, languageMapping)
}

object WorldQueryCompiler extends QueryCompiler(WorldData.schema) {
  val QueryType = WorldData.schema.ref("Query")

  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(Binding("code", StringValue(code))), child) =>
        Select("country", Nil, Unique(Eql(AttrPath(List("code")), Const(code)), child)).rightIor
      case Select("countries", Nil, child) =>
        Select("countries", Nil, child).rightIor
      case Select("cities", List(Binding("namePattern", StringValue(namePattern))), child) =>
        Select("cities", Nil, Filter(Like(FieldPath(List("name")), namePattern, true), child)).rightIor
      case Select("language", List(Binding("language", StringValue(language))), child) =>
        Select("language", Nil, Unique(Eql(FieldPath(List("language")), Const(language)), child)).rightIor
      case Select("languages", Nil, child) =>
        Select("languages", Nil, child).rightIor
      case Select("search", List(Binding("minPopulation", IntValue(min)), Binding("indepSince", IntValue(year))), child) =>
        Select("search", Nil,
          Filter(
            And(
              Not(Lt(FieldPath(List("population")), Const(min))),
              Not(Lt(FieldPath(List("indepyear")), Const(year)))
            ),
            child
          )
        ).rightIor
    }
  ))

  val stagingElaborator = new StagingElaborator(WorldData)

  val phases = List(selectElaborator, stagingElaborator)
}

object WorldQueryInterpreter {
  def fromTransactor[F[_]](xa: Transactor[F])
    (implicit brkt: Bracket[F, Throwable], logger: Logger[F]): DoobieQueryInterpreter[F] =
      new DoobieQueryInterpreter[F](WorldData, xa, logger)
}
