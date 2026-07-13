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

import grackle._
import grackle.Predicate.{Const, Eql}
import grackle.Query.{Binding, Filter, Unique}
import grackle.QueryCompiler.{Elab, SelectElaborator}
import grackle.Value.StringValue
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

  val schema =
    schema"""
      type Query {
        country(code: String!): Country
      }
      type Country {
        code: String!
        name: String!
        cities: [City!]!
      }
      type City {
        name: String!
        country: Country!
      }
    """

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")
  val CityType = schema.ref("City")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlObject("country")
        )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings = List(
          SqlField("code", country.code, key = true),
          SqlField("name", country.name),
          SqlObject("cities", Join(country.code, city.countrycode))
        )
      ),
      ObjectMapping(
        tpe = CityType,
        fieldMappings = List(
          SqlField("id", city.id, key = true, hidden = true),
          SqlField("countrycode", city.countrycode, hidden = true),
          SqlField("name", city.name),
          SqlObject("country", Join(city.countrycode, country.code))
        )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "country", List(Binding("code", StringValue(code)))) =>
      Elab.transformChild(child =>
        Unique(Filter(Eql(CountryType / "code", Const(code)), child)))
  }
}
