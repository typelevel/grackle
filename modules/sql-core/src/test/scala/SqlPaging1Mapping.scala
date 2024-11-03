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

package grackle.sql.test

import cats.implicits._

import grackle._, syntax._
import Query.{Binding, Count, FilterOrderByOffsetLimit, OrderSelection, Select}
import QueryCompiler.{Elab, SelectElaborator}
import Value.IntValue

// Mapping illustrating paging in "counted" style: paged results can
// report the current offet, limit and total number of items in the
// underlying list.
//
// This implementation will only ever fetch up to the requested number
// of items, and will include an SQL COUNT if the query includes `total`.
trait SqlPaging1Mapping[F[_]] extends SqlTestMapping[F] {
  object root extends RootDef {
    val numCountries = col("num_countries", int8)
  }

  object country extends TableDef("country") {
    val code         = col("code", bpchar(3))
    val name         = col("name", nvarchar)
    val numCities    = col("num_cities", int8)
  }

  object city extends TableDef("city") {
    val id          = col("id", int4)
    val countrycode = col("countrycode", bpchar(3))
    val name        = col("name", nvarchar)
  }

  val schema =
    schema"""
      type Query {
        countries(offset: Int!, limit: Int!): PagedCountry!
      }
      type PagedCountry {
        offset: Int!
        limit: Int!
        total: Int!
        items: [Country!]!
      }
      type Country {
        code: String!
        name: String!
        cities(offset: Int!, limit: Int!): PagedCity!
      }
      type PagedCity {
        offset: Int!
        limit: Int!
        total: Int!
        items: [City!]!
      }
      type City {
        name: String!
      }
    """

  val QueryType = schema.ref("Query")
  val PagedCountryType = schema.ref("PagedCountry")
  val CountryType = schema.ref("Country")
  val PagedCityType = schema.ref("PagedCity")
  val CityType = schema.ref("City")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlObject("countries"),
          )
      ),
      ObjectMapping(
        tpe = PagedCountryType,
        fieldMappings =
          List(
            CursorField("offset", CountryPaging.genOffset, Nil),
            CursorField("limit", CountryPaging.genLimit, Nil),
            SqlField("total", root.numCountries),
            SqlObject("items")
          )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings =
          List(
            SqlField("code", country.code, key = true),
            SqlField("name", country.name),
            SqlObject("cities")
          )
      ),
      ObjectMapping(
        tpe = PagedCityType,
        fieldMappings =
          List(
            SqlField("code", country.code, key = true, hidden = true),
            SqlObject("items", Join(country.code, city.countrycode)),
            CursorField("offset", CityPaging.genOffset, Nil),
            CursorField("limit", CityPaging.genLimit, Nil),
            SqlField("total", country.numCities),
          )
      ),
      ObjectMapping(
        tpe = CityType,
        fieldMappings =
          List(
            SqlField("id", city.id, key = true, hidden = true),
            SqlField("countrycode", city.countrycode, hidden = true),
            SqlField("name", city.name)
          )
      )
    )

  def genValue(key: String)(c: Cursor): Result[Int] =
    c.env[Int](key).toResultOrError(s"Missing key '$key'")

  abstract class PagingConfig(key: String, countAttr: String, orderTerm: Term[String]) {
    def setup(offset: Int, limit: Int): Elab[Unit] =
      Elab.env(key -> new PagingInfo(offset, limit))

    def elabItems = Elab.envE[PagingInfo](key).flatMap(_.elabItems)
    def elabTotal = Elab.envE[PagingInfo](key).flatMap(_.elabTotal)

    def genOffset(c: Cursor): Result[Int] = info(c, _.offset)
    def genLimit(c: Cursor): Result[Int] = info(c, _.limit)

    def info(c: Cursor, f: PagingInfo => Int): Result[Int] =
      c.env[PagingInfo](key).map(f).toResultOrError(s"Missing key '$key'")

    case class PagingInfo(offset: Int, limit: Int) {
      def elabItems: Elab[Unit] =
        Elab.transformChild { child =>
          FilterOrderByOffsetLimit(None, Some(List(OrderSelection(orderTerm, nullsLast = nullsHigh))), Some(offset), Some(limit), child)
        }

      def elabTotal: Elab[Unit] =
        Elab.transformChild(_ => Count(Select("items", Select(countAttr))))
    }
  }

  object CountryPaging extends PagingConfig("countryPaging", "code", CountryType / "code")
  object CityPaging extends PagingConfig("cityPaging", "id", CityType / "name")

  override val selectElaborator = SelectElaborator {
    case (QueryType, "countries", List(Binding("offset", IntValue(off)), Binding("limit", IntValue(lim)))) =>
      CountryPaging.setup(off, lim)

    case (PagedCountryType, "items", Nil) =>
      CountryPaging.elabItems

    case (PagedCountryType, "total", Nil) =>
      CountryPaging.elabTotal

    case (CountryType, "cities", List(Binding("offset", IntValue(off)), Binding("limit", IntValue(lim)))) =>
      CityPaging.setup(off, lim)

    case (PagedCityType, "items", Nil) =>
      CityPaging.elabItems

    case (PagedCityType, "total", Nil) =>
      CityPaging.elabTotal
  }
}
