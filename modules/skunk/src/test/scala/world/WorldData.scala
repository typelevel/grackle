// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.effect.Sync
import cats.implicits._

import edu.gemini.grackle._, skunk._
import Query._, Predicate._, Value._
import QueryCompiler._
import SkunkPredicate._
import _root_.skunk.codec.all._
import cats.effect.Resource
import _root_.skunk.Session

trait WorldMapping[F[_]] extends SkunkMapping[F] {
  val schema =
    Schema(
      """
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
    ).right.get

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")
  val CityType = schema.ref("City")
  val LanguageType = schema.ref("Language")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SkunkRoot("cities"),
            SkunkRoot("country"),
            SkunkRoot("countries"),
            SkunkRoot("language"),
            SkunkRoot("search")
          )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings =
          List(
            SkunkAttribute("code", ColumnRef("country", "code", bpchar(3)), bpchar(3), key = true),
            SkunkField("name", ColumnRef("country", "name", text)),
            SkunkField("continent", ColumnRef("country", "continent", varchar)),
            SkunkField("region", ColumnRef("country", "region", varchar)),
            SkunkField("surfacearea", ColumnRef("country", "surfacearea", varchar)),
            SkunkField("indepyear", ColumnRef("country", "indepyear", int2.imap(_.toInt)(_.toShort).opt)),
            SkunkField("population", ColumnRef("country", "population", int4)),
            SkunkField("lifeexpectancy", ColumnRef("country", "lifeexpectancy", varchar)),
            SkunkField("gnp", ColumnRef("country", "gnp", varchar)),
            SkunkField("gnpold", ColumnRef("country", "gnpold", varchar)),
            SkunkField("localname", ColumnRef("country", "localname", varchar)),
            SkunkField("governmentform", ColumnRef("country", "governmentform", varchar)),
            SkunkField("headofstate", ColumnRef("country", "headofstate", varchar)),
            SkunkField("capitalId", ColumnRef("country", "capitalId", varchar)),
            SkunkField("code2", ColumnRef("country", "code2", varchar)),
            SkunkObject("cities", Subobject(
              List(Join(ColumnRef("country", "code", bpchar(3)), ColumnRef("city", "countrycode", bpchar(3))))
            )),
            SkunkObject("languages", Subobject(
              List(Join(ColumnRef("country", "code", bpchar(3)), ColumnRef("countryLanguage", "countrycode", bpchar(3))))
            ))
          ),
      ),
      ObjectMapping(
        tpe = CityType,
        fieldMappings =
          List(
            SkunkAttribute("id", ColumnRef("city", "id", varchar), int4, key = true),
            SkunkAttribute("countrycode", ColumnRef("city", "countrycode", bpchar(3)), bpchar(3)),
            SkunkField("name", ColumnRef("city", "name", text)),
            SkunkObject("country", Subobject(
              List(Join(ColumnRef("city", "countrycode", bpchar(3)), ColumnRef("country", "code", bpchar(3))))
            )),
            SkunkField("district", ColumnRef("city", "district", varchar)),
            SkunkField("population", ColumnRef("city", "population", int4))
          )
      ),
      ObjectMapping(
        tpe = LanguageType,
        fieldMappings =
          List(
            SkunkField("language", ColumnRef("countryLanguage", "language", text), key = true),
            SkunkField("isOfficial", ColumnRef("countryLanguage", "isOfficial", varchar)),
            SkunkField("percentage", ColumnRef("countryLanguage", "percentage", varchar)),
            SkunkAttribute("countrycode", ColumnRef("countryLanguage", "countrycode", bpchar(3)), bpchar(3)),
            SkunkObject("countries", Subobject(
              List(Join(ColumnRef("countryLanguage", "countrycode", bpchar(3)), ColumnRef("country", "code", bpchar(3))))
            ))
          )
      )
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("country", List(Binding("code", StringValue(code))), child) =>
        Select("country", Nil, Unique(Eql(AttrPath(List("code")), Const(code)), child)).rightIor

      case Select("countries", List(Binding("limit", IntValue(num)), Binding("minPopulation", IntValue(min)), Binding("byPopulation", BooleanValue(byPop))), child) =>
        def limit(query: Query): Query =
          if (num < 1) query
          else Limit(num, query)

        def order(query: Query): Query =
          if (byPop) OrderBy(OrderSelections(List(OrderSelection(FieldPath[Int](List("population"))))), query)
          else query

        def filter(query: Query): Query =
          if (min == 0) query
          else Filter(GtEql(FieldPath(List("population")), Const(min)), query)

        Select("countries", Nil, limit(order(filter(child)))).rightIor

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

object WorldMapping extends SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F,Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with WorldMapping[F]

}
