// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.implicits._

import edu.gemini.grackle._
import sql.Like
import syntax._
import Query._, Predicate._, Value._
import QueryCompiler._

trait SqlWorldMapping[F[_]] extends SqlTestMapping[F] {
  object root extends RootDef {
    val numCountries = col("num_countries", int8)
  }
  object country extends TableDef("country") {
    val code           = col("code", bpchar(3))
    val name           = col("name", text)
    val continent      = col("continent", text)
    val region         = col("region", text)
    val surfacearea    = col("surfacearea", float4)
    val indepyear      = col("indepyear", nullable(int2))
    val population     = col("population", int4)
    val lifeexpectancy = col("lifeexpectancy", nullable(float4))
    val gnp            = col("gnp", nullable(numeric(10, 2)))
    val gnpold         = col("gnpold", nullable(numeric(10, 2)))
    val localname      = col("localname", text)
    val governmentform = col("governmentform", text)
    val headofstate    = col("headofstate", nullable(text))
    val capitalId      = col("capital", nullable(int4))
    val numCities      = col("num_cities", int8)
    val code2          = col("code2", bpchar(2))
  }

  object city extends TableDef("city") {
    val id          = col("id", int4)
    val countrycode = col("countrycode", bpchar(3))
    val name        = col("name", text)
    val district    = col("district", text)
    val population  = col("population", int4)
  }

  object countrylanguage extends TableDef("countrylanguage") {
    val countrycode = col("countrycode", bpchar(3))
    val language = col("language", text)
    val isOfficial = col("isOfficial", bool)
    val percentage = col("percentage", float4)
  }

  val schema =
    schema"""
      type Query {
        cities(namePattern: String = "%"): [City!]
        city(id: Int): City
        country(code: String): Country
        countries(limit: Int = -1, offset: Int = 0, minPopulation: Int = 0, byPopulation: Boolean = false): [Country!]
        language(language: String): Language
        languages(languages: [String!]!): [Language!]!
        search(minPopulation: Int!, indepSince: Int!): [Country!]!
        search2(indep: Boolean!, limit: Int!): [Country!]!
        numCountries: Int!
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
        gnp: Float
        gnpold: Float
        localname: String!
        governmentform: String!
        headofstate: String
        capitalId: Int
        code2: String!
        numCities(namePattern: String): Int!
        cities: [City!]!
        city(id: Int): City
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
          SqlObject("cities"),
          SqlObject("city"),
          SqlObject("country"),
          SqlObject("countries"),
          SqlObject("language"),
          SqlObject("languages"),
          SqlObject("search"),
          SqlObject("search2"),
          SqlField("numCountries", root.numCountries)
        )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings = List(
          SqlField("code",           country.code, key = true, hidden = true),
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
          SqlField("numCities",      country.numCities),
          SqlObject("cities",        Join(country.code, city.countrycode)),
          SqlObject("city",          Join(country.code, city.countrycode)),
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

  object StringListValue {
    def unapply(value: Value): Option[List[String]] =
      value match {
        case ListValue(l) => l.traverse {
          case StringValue(s) => Some(s)
          case _ => None
        }
        case _ => None
      }
  }

  override val selectElaborator = SelectElaborator {
    case (QueryType, "country", List(Binding("code", StringValue(code)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CountryType / "code", Const(code)), child)))

    case (QueryType, "city", List(Binding("id", IntValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CityType / "id", Const(id)), child)))

    case (QueryType, "countries", List(Binding("limit", IntValue(num)), Binding("offset", IntValue(off)), Binding("minPopulation", IntValue(min)), Binding("byPopulation", BooleanValue(byPop)))) =>
      def limit(query: Query): Query =
        if (num < 1) query
        else Limit(num, query)

      def offset(query: Query): Query =
        if (off < 1) query
        else Offset(off, query)

      def order(query: Query): Query = {
        if (byPop)
          OrderBy(OrderSelections(List(OrderSelection[Int](CountryType / "population"))), query)
        else if (num > 0 || off > 0)
          OrderBy(OrderSelections(List(OrderSelection[String](CountryType / "code"))), query)
        else query
      }

      def filter(query: Query): Query =
        if (min == 0) query
        else Filter(GtEql(CountryType / "population", Const(min)), query)

      Elab.transformChild(child => limit(offset(order(filter(child)))))

    case (QueryType, "cities", List(Binding("namePattern", StringValue(namePattern)))) =>
      if (namePattern == "%")
        Elab.unit
      else
        Elab.transformChild(child => Filter(Like(CityType / "name", namePattern, true), child))

    case (QueryType, "language", List(Binding("language", StringValue(language)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(LanguageType / "language", Const(language)), child)))

    case (QueryType, "languages", List(Binding("languages", StringListValue(languages)))) =>
      Elab.transformChild(child => Filter(In(CityType / "language", languages), child))

    case (QueryType, "search", List(Binding("minPopulation", IntValue(min)), Binding("indepSince", IntValue(year)))) =>
      Elab.transformChild(child =>
        Filter(
          And(
            Not(Lt(CountryType / "population", Const(min))),
            Not(Lt(CountryType / "indepyear", Const(Option(year))))
          ),
          child
        )
      )

    case (QueryType, "search2", List(Binding("indep", BooleanValue(indep)), Binding("limit", IntValue(num)))) =>
      Elab.transformChild(child => Limit(num, Filter(IsNull[Int](CountryType / "indepyear", isNull = !indep), child)))

    case (QueryType, "numCountries", Nil) =>
      Elab.transformChild(_ => Count(Select("countries", Select("code2"))))

    case (CountryType, "numCities", List(Binding("namePattern", AbsentValue))) =>
      Elab.transformChild(_ => Count(Select("cities", Select("name"))))

    case (CountryType, "numCities", List(Binding("namePattern", StringValue(namePattern)))) =>
      Elab.transformChild(_ => Count(Select("cities", Filter(Like(CityType / "name", namePattern, true), Select("name")))))

    case (CountryType, "city", List(Binding("id", IntValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CityType / "id", Const(id)), child)))
  }
}
