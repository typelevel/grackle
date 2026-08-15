// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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

package grackle.sql.test

import grackle.Predicate.{Const, Eql}
import grackle.Query.{Binding, Filter, Limit, OrderBy, OrderSelection, OrderSelections, Unique}
import grackle.QueryCompiler.{Elab, SelectElaborator}
import grackle.Value.{IntValue, StringValue}
import grackle.syntax._

// Mapping over tables with schema-qualified names (issue #342). The City -> Country
// relationship closes a cycle, so a query traversing Country -> City -> Country revisits
// the country table and forces the alias machinery to mint an alias for a qualified name.
trait SqlQualifiedNamesMapping[F[_]] extends SqlTestMapping[F] {

  object country extends TableDef("qualified.country") {
    val code = col("code", bpchar(3))
    val name = col("name", text)
  }

  object city extends TableDef("qualified.city") {
    val id = col("id", int4)
    val countrycode = col("countrycode", bpchar(3))
    val name = col("name", text)
  }

  object speaks extends TableDef("qualified.speaks") {
    val countrycode = col("countrycode", bpchar(3))
    val lang = col("lang", text)
  }

  // Named so that folding qualified.country's qualifier with an underscore yields exactly
  // this table's name, pinning that synthesized identifiers and real tables coexist.
  object twin extends TableDef("qualified_country") {
    val code = col("code", bpchar(3))
    val motto = col("motto", text)
  }

  val schema =
    schema"""
      type Query {
        country(code: String!): Country
        countries(limit: Int!): [Country!]!
      }
      type Country {
        code: String!
        name: String!
        cities(limit: Int): [City!]!
        languages: [Language!]!
        twin: Twin
      }
      type City {
        name: String!
        country: Country!
        languages: [Language!]!
        twins: [Twin!]!
      }
      type Language {
        language: String!
      }
      type Twin {
        motto: String!
      }
    """

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")
  val CityType = schema.ref("City")
  val LanguageType = schema.ref("Language")
  val TwinType = schema.ref("Twin")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlObject("country"),
          SqlObject("countries")
        )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings = List(
          SqlField("code", country.code, key = true),
          SqlField("name", country.name),
          SqlObject("cities", Join(country.code, city.countrycode)),
          SqlObject("languages", Join(country.code, speaks.countrycode)),
          SqlObject("twin", Join(country.code, twin.code))
        )
      ),
      ObjectMapping(
        tpe = CityType,
        fieldMappings = List(
          SqlField("id", city.id, key = true, hidden = true),
          SqlField("countrycode", city.countrycode, hidden = true),
          SqlField("name", city.name),
          SqlObject("country", Join(city.countrycode, country.code)),
          // Two sibling list children of City. A limit on the parent `cities` field compiles
          // these to the branches of an SqlUnion, which is the shape that exercises
          // SqlUnion.addFilterOrderByOffsetLimit's subquery naming.
          SqlObject("languages", Join(city.countrycode, speaks.countrycode)),
          SqlObject("twins", Join(city.countrycode, twin.code))
        )
      ),
      ObjectMapping(
        tpe = LanguageType,
        fieldMappings = List(
          SqlField("language", speaks.lang, key = true, associative = true),
          SqlField("countrycode", speaks.countrycode, hidden = true)
        )
      ),
      ObjectMapping(
        tpe = TwinType,
        fieldMappings = List(
          SqlField("code", twin.code, key = true, hidden = true),
          SqlField("motto", twin.motto)
        )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "country", List(Binding("code", StringValue(code)))) =>
      Elab.transformChild(child =>
        Unique(Filter(Eql(CountryType / "code", Const(code)), child)))

    case (QueryType, "countries", List(Binding("limit", IntValue(limit)))) =>
      Elab.transformChild(child =>
        Limit(
          limit,
          OrderBy(OrderSelections(List(OrderSelection[String](CountryType / "code"))), child)))

    case (CountryType, "cities", List(Binding("limit", limit))) =>
      Elab.transformChild(child =>
        limit match {
          case IntValue(lim) =>
            Limit(
              lim,
              OrderBy(OrderSelections(List(OrderSelection[String](CityType / "name"))), child))
          case _ => child
        })
  }
}
