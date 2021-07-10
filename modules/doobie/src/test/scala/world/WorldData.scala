// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.Sync
import cats.implicits._

import doobie.util.meta.Meta
import doobie.util.transactor.Transactor

import edu.gemini.grackle._
import doobie.{DoobieMapping, DoobieMappingCompanion, DoobieMonitor}
import sql.Like
import syntax._
import Query._, Path._, Predicate._, Value._
import QueryCompiler._

trait WorldPostgresSchema[F[_]] extends DoobieMapping[F] {

  object country extends TableDef("country") {
    val code           = col("code", Meta[String])
    val name           = col("name", Meta[String])
    val continent      = col("continent", Meta[String])
    val region         = col("region", Meta[String])
    val surfacearea    = col("surfacearea", Meta[String])
    val indepyear      = col("indepyear", Meta[Int], true)
    val population     = col("population", Meta[Int])
    val lifeexpectancy = col("lifeexpectancy", Meta[String], true)
    val gnp            = col("gnp", Meta[String], true)
    val gnpold         = col("gnpold", Meta[String], true)
    val localname      = col("localname", Meta[String])
    val governmentform = col("governmentform", Meta[String])
    val headofstate    = col("headofstate", Meta[String], true)
    val capitalId      = col("capitalId", Meta[String], true)
    val code2          = col("code2", Meta[String])
  }

  object city extends TableDef("city") {
    val id          = col("id", Meta[Int])
    val countrycode = col("countrycode", Meta[String])
    val name        = col("name", Meta[String])
    val district    = col("district", Meta[String])
    val population  = col("population", Meta[Int])
  }

  object countrylanguage extends TableDef("countrylanguage") {
    val countrycode = col("countrycode", Meta[String])
    val language = col("language", Meta[String])
    val isOfficial = col("isOfficial", Meta[String])
    val percentage = col("percentage", Meta[String])
  }

}

trait WorldMapping[F[_]] extends WorldPostgresSchema[F] {

  val schema =
    schema"""
      type Query {
        cities(namePattern: String = "%"): [City!]
        country(code: String): Country
        countries(limit: Int = -1, minPopulation: Int = 0, byPopulation: Boolean = false): [Country!]
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

  val QueryType    = schema.ref("Query")
  val CountryType  = schema.ref("Country")
  val CityType     = schema.ref("City")
  val LanguageType = schema.ref("Language")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlRoot("cities"),
          SqlRoot("country"),
          SqlRoot("countries"),
          SqlRoot("language"),
          SqlRoot("search")
        )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings = List(
          SqlField("code",       country.code, key = true, hidden = true),
          SqlField("name",           country.name),
          SqlField("continent",      country.continent),
          SqlField("region",         country.region),
          SqlField("surfacearea",    country.surfacearea),
          SqlField("indepyear",      country.indepyear),
          SqlField("population",     country.population),
          SqlField("lifeexpectancy", country.lifeexpectancy),
          SqlField("gnp",            country.gnp),
          SqlField("gnpold",         country.gnpold),
          SqlField("localname",      country.localname),
          SqlField("governmentform", country.governmentform),
          SqlField("headofstate",    country.headofstate),
          SqlField("capitalId",      country.capitalId),
          SqlField("code2",          country.code2),
          SqlObject("cities",        Join(country.code, city.countrycode)),
          SqlObject("languages",     Join(country.code, countrylanguage.countrycode))
        ),
      ),
      ObjectMapping(
        tpe = CityType,
        fieldMappings = List(
          SqlField("id", city.id, key = true, hidden = true),
          SqlField("countrycode", city.countrycode, hidden = true),
          SqlField("name", city.name),
          SqlField("district", city.district),
          SqlField("population", city.population),
          SqlObject("country", Join(city.countrycode, country.code)),
        )
      ),
      ObjectMapping(
        tpe = LanguageType,
        fieldMappings = List(
          SqlField("language", countrylanguage.language, key = true, associative = true),
          SqlField("isOfficial", countrylanguage.isOfficial),
          SqlField("percentage", countrylanguage.percentage),
          SqlField("countrycode", countrylanguage.countrycode, hidden = true),
          SqlObject("countries", Join(countrylanguage.countrycode, country.code))
        )
      )
    )

  override val selectElaborator = new SelectElaborator(Map(

    QueryType -> {

      case Select("country", List(Binding("code", StringValue(code))), child) =>
        Select("country", Nil, Unique(Filter(Eql(UniquePath(List("code")), Const(code)), child))).rightIor

      case Select("countries", List(Binding("limit", IntValue(num)), Binding("minPopulation", IntValue(min)), Binding("byPopulation", BooleanValue(byPop))), child) =>
        def limit(query: Query): Query =
          if (num < 1) query
          else Limit(num, query)

        def order(query: Query): Query =
          if (byPop) OrderBy(OrderSelections(List(OrderSelection(UniquePath[Int](List("population"))))), query)
          else query

        def filter(query: Query): Query =
          if (min == 0) query
          else Filter(GtEql(UniquePath(List("population")), Const(min)), query)

        Select("countries", Nil, limit(order(filter(child)))).rightIor

      case Select("cities", List(Binding("namePattern", StringValue(namePattern))), child) =>
        Select("cities", Nil, Filter(Like(UniquePath(List("name")), namePattern, true), child)).rightIor

      case Select("language", List(Binding("language", StringValue(language))), child) =>
        Select("language", Nil, Unique(Filter(Eql(UniquePath(List("language")), Const(language)), child))).rightIor

      case Select("search", List(Binding("minPopulation", IntValue(min)), Binding("indepSince", IntValue(year))), child) =>
        Select("search", Nil,
          Filter(
            And(
              Not(Lt(UniquePath(List("population")), Const(min))),
              Not(Lt(UniquePath(List("indepyear")), Const(Option(year))))
            ),
            child
          )
        ).rightIor

    }
  ))
}

object WorldMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with WorldMapping[F]

}
