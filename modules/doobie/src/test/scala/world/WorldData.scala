// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.Sync
import cats.implicits._
import doobie.Transactor
import io.chrisdavenport.log4cats.Logger

import edu.gemini.grackle._, doobie._
import Query._, Predicate._, Value._
import QueryCompiler._
import QueryInterpreter.mkErrorResult
import DoobiePredicate._

class WorldMapping[F[_]: Sync](val transactor: Transactor[F], val logger: Logger[F]) extends DoobieMapping[F] {
  val schema =
    Schema(
      """
        type Query {
          cities(namePattern: String = "%"): [City!]
          country(code: String): Country
          countries: [Country!]
          language(language: String): Language
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

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")
  val CityType = schema.ref("City")
  val LanguageType = schema.ref("Language")

  import DoobieFieldMapping._

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            DoobieRoot("cities"),
            DoobieRoot("country"),
            DoobieRoot("countries"),
            DoobieRoot("language"),
            DoobieRoot("search")
          )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings =
          List(
            DoobieAttribute[String]("code", ColumnRef("country", "code"), key = true),
            DoobieField("name", ColumnRef("country", "name")),
            DoobieField("continent", ColumnRef("country", "continent")),
            DoobieField("region", ColumnRef("country", "region")),
            DoobieField("surfacearea", ColumnRef("country", "surfacearea")),
            DoobieField("indepyear", ColumnRef("country", "indepyear")),
            DoobieField("population", ColumnRef("country", "population")),
            DoobieField("lifeexpectancy", ColumnRef("country", "lifeexpectancy")),
            DoobieField("gnp", ColumnRef("country", "gnp")),
            DoobieField("gnpold", ColumnRef("country", "gnpold")),
            DoobieField("localname", ColumnRef("country", "localname")),
            DoobieField("governmentform", ColumnRef("country", "governmentform")),
            DoobieField("headofstate", ColumnRef("country", "headofstate")),
            DoobieField("capitalId", ColumnRef("country", "capitalId")),
            DoobieField("code2", ColumnRef("country", "code2")),
            DoobieObject("cities", Subobject(
              List(Join(ColumnRef("country", "code"), ColumnRef("city", "countrycode"))),
              countryCityJoin
            )),
            DoobieObject("languages", Subobject(
              List(Join(ColumnRef("country", "code"), ColumnRef("countryLanguage", "countrycode"))),
              countryLanguageJoin
            )),
          ),
      ),
      ObjectMapping(
        tpe = CityType,
        fieldMappings =
          List(
            DoobieAttribute[Int]("id", ColumnRef("city", "id"), key = true),
            DoobieAttribute[String]("countrycode", ColumnRef("city", "countrycode")),
            DoobieField("name", ColumnRef("city", "name")),
            DoobieObject("country", Subobject(
              List(Join(ColumnRef("city", "countrycode"), ColumnRef("country", "code"))),
              cityCountryJoin
            )),
            DoobieField("district", ColumnRef("city", "district")),
            DoobieField("population", ColumnRef("city", "population"))
          )
      ),
      ObjectMapping(
        tpe = LanguageType,
        fieldMappings =
          List(
            DoobieField("language", ColumnRef("countryLanguage", "language"), key = true),
            DoobieField("isOfficial", ColumnRef("countryLanguage", "isOfficial")),
            DoobieField("percentage", ColumnRef("countryLanguage", "percentage")),
            DoobieAttribute[String]("countrycode", ColumnRef("countryLanguage", "countrycode")),
            DoobieObject("countries", Subobject(
              List(Join(ColumnRef("countryLanguage", "countrycode"), ColumnRef("country", "code"))),
              languageCountryJoin
            ))
          )
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

  def cityCountryJoin(c: Cursor, q: Query): Result[Query] = q match {
    case Select("country", Nil, child) =>
      c.attribute("countrycode").map { case (countrycode: String) =>
        Select("country", Nil, Unique(Eql(AttrPath(List("code")), Const(countrycode)), child))
      }
    case _ => mkErrorResult("Bad staging join")
  }

  def languageCountryJoin(c: Cursor, q: Query): Result[Query] = q match {
    case Select("countries", Nil, child) =>
      c.field("language").map { case ScalarFocus(language: String) =>
        Select("countries", Nil, Filter(Contains(FieldPath(List("languages", "language")), Const(language)), child))
      }
    case _ => mkErrorResult("Bad staging join")
  }

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(Binding("code", StringValue(code))), child) =>
        Select("country", Nil, Unique(Eql(AttrPath(List("code")), Const(code)), child)).rightIor
      case Select("countries", Nil, child) =>
        Select("countries", Nil, child).rightIor
      case Select("cities", List(Binding("namePattern", StringValue(namePattern))), child) =>
        Select("cities", Nil, Filter(Like(FieldPath(List("name")), namePattern, true), child)).rightIor
      case Select("language", List(Binding("language", StringValue(language))), child) =>
        Select("language", Nil, Unique(Eql(FieldPath(List("language")), Const(language)), child)).rightIor
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
}

object WorldMapping {
  def fromTransactor[F[_] : Sync : Logger](transactor: Transactor[F]): WorldMapping[F] =
    new WorldMapping[F](transactor, Logger[F])
}
