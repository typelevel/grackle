// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package demo.world

import _root_.doobie.{Meta, Transactor}
import cats.effect.Sync
import grackle.Predicate._
import grackle.Query._
import grackle.QueryCompiler._
import grackle.Value._
import grackle._
import grackle.doobie.postgres.{DoobieMapping, DoobieMonitor, LoggedDoobieMappingCompanion}
import grackle.sql.Like
import grackle.syntax._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait WorldMapping[F[_]] extends DoobieMapping[F] {
  // #db_tables
  object country extends TableDef("country") {
    val code           = col("code", Meta[String])
    val name           = col("name", Meta[String])
    val continent      = col("continent", Meta[String])
    val region         = col("region", Meta[String])
    val surfacearea    = col("surfacearea", Meta[Float])
    val indepyear      = col("indepyear", Meta[Int], nullable = true)
    val population     = col("population", Meta[Int])
    val lifeexpectancy = col("lifeexpectancy", Meta[Float], nullable = true)
    val gnp            = col("gnp", Meta[BigDecimal], nullable = true)
    val gnpold         = col("gnpold", Meta[BigDecimal], nullable = true)
    val localname      = col("localname", Meta[String])
    val governmentform = col("governmentform", Meta[String])
    val headofstate    = col("headofstate", Meta[String], nullable = true)
    val capitalId      = col("capital", Meta[Int], nullable = true)
    val numCities      = col("num_cities", Meta[Long])
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
    val isOfficial = col("isOfficial", Meta[Boolean])
    val percentage = col("percentage", Meta[Float])
  }
  // #db_tables

  // #schema
  val schema =
    schema"""
      type Query {
        cities(namePattern: String = "%"): [City!]
        city(id: Int): City
        country(code: String): Country
        countries(limit: Int = -1, offset: Int = 0, minPopulation: Int = 0,
                  byPopulation: Boolean = false): [Country!]
        language(language: String): Language
        search(minPopulation: Int!, indepSince: Int!): [Country!]!
        search2(indep: Boolean!, limit: Int!): [Country!]!
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
        code: String!
        code2: String!
        numCities(namePattern: String): Int!
        cities: [City!]!
        languages: [Language!]!
      }
    """
  // #schema

  val QueryType    = schema.ref("Query")
  val CountryType  = schema.ref("Country")
  val CityType     = schema.ref("City")
  val LanguageType = schema.ref("Language")

  val typeMappings =
    TypeMappings(
      // #root
      ObjectMapping(QueryType)(
        SqlObject("cities"),
        SqlObject("city"),
        SqlObject("country"),
        SqlObject("countries"),
        SqlObject("language"),
        SqlObject("search"),
        SqlObject("search2")
      ),
      // #root
      // #type_mappings
      ObjectMapping(CountryType)(
        SqlField("code",           country.code, key = true),
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
        SqlObject("languages",     Join(country.code, countrylanguage.countrycode))
      ),
      ObjectMapping(CityType)(
        SqlField("id", city.id, key = true, hidden = true),
        SqlField("countrycode", city.countrycode, hidden = true),
        SqlField("name", city.name),
        SqlField("district", city.district),
        SqlField("population", city.population),
        SqlObject("country", Join(city.countrycode, country.code)),
      ),
      ObjectMapping(LanguageType)(
        SqlField("language", countrylanguage.language, key = true, associative = true),
        SqlField("isOfficial", countrylanguage.isOfficial),
        SqlField("percentage", countrylanguage.percentage),
        SqlField("countrycode", countrylanguage.countrycode, hidden = true),
        SqlObject("countries", Join(countrylanguage.countrycode, country.code))
      )
      // #type_mappings
    )

  // #elaborator
  override val selectElaborator = SelectElaborator {
    case (QueryType, "country", List(Binding("code", StringValue(code)))) =>
      Elab.transformChild { child =>
        Unique(Filter(Eql(CountryType / "code", Const(code)), child))
      }

    case (QueryType, "city", List(Binding("id", IntValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CityType / "id", Const(id)), child)))

    case (
           QueryType, "countries",
           List(
             Binding("limit", IntValue(num)),
             Binding("offset", IntValue(off)),
             Binding("minPopulation", IntValue(min)),
             Binding("byPopulation", BooleanValue(byPop))
           )
         ) =>
      def limit(query: Query): Query =
        if (num < 1) query
        else Limit(num, query)

      def offset(query: Query): Query =
        if (off < 1) query
        else Offset(off, query)

      def order(query: Query): Query = {
        if (byPop)
          OrderBy(
            OrderSelections(List(OrderSelection[Int](CountryType / "population"))),
            query
          )
        else if (num > 0 || off > 0)
          OrderBy(
            OrderSelections(List(OrderSelection[String](CountryType / "code"))),
            query
          )
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
        Elab.transformChild { child =>
          Filter(Like(CityType / "name", namePattern, true), child)
        }

    case (QueryType, "language", List(Binding("language", StringValue(language)))) =>
      Elab.transformChild { child =>
        Unique(Filter(Eql(LanguageType / "language", Const(language)), child))
      }

    case (QueryType, "search",
           List(
             Binding("minPopulation", IntValue(min)),
             Binding("indepSince", IntValue(year))
            )
          ) =>
      Elab.transformChild(child =>
        Filter(
          And(
            Not(Lt(CountryType / "population", Const(min))),
            Not(Lt(CountryType / "indepyear", Const(Option(year))))
          ),
          child
        )
      )

    case (QueryType, "search2",
           List(
             Binding("indep", BooleanValue(indep)),
             Binding("limit", IntValue(num))
           )
         ) =>
      Elab.transformChild { child =>
        Limit(num, Filter(IsNull[Int](CountryType / "indepyear", isNull = !indep), child))
      }

    case (CountryType, "numCities", List(Binding("namePattern", AbsentValue))) =>
      Elab.transformChild { _ =>
        Count(Select("cities", Select("name")))
      }

    case (CountryType, "numCities",
           List(Binding("namePattern", StringValue(namePattern)))) =>
      Elab.transformChild { _ =>
        Count(
          Select("cities",
            Filter(Like(CityType / "name", namePattern, true), Select("name"))
          )
        )
      }
  }
  // #elaborator
}

object WorldMapping extends LoggedDoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): WorldMapping[F] =
    new DoobieMapping(transactor, monitor) with WorldMapping[F]

  def mkMappingFromTransactor[F[_]: Sync](transactor: Transactor[F]): Mapping[F] = {
    implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F]("SqlQueryLogger")
    mkMapping(transactor)
  }
}
