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
import Value._

// Mapping illustrating paging in "has more" style: paged results can
// report whether there are more elements beyond the current sub list.
//
// This implmentation will only ever fetch up to the requested number
// of items, and will include an SQL COUNT if the query includes `hasMore`.
trait SqlPaging2Mapping[F[_]] extends SqlTestMapping[F] {
  object root extends RootDef {
    val numCountries = col("num_countries", int8)
  }

  object country extends TableDef("country") {
    val code      = col("code", bpchar(3))
    val name      = col("name", text)
    val numCities = col("num_cities", int8)
  }

  object city extends TableDef("city") {
    val id          = col("id", int4)
    val countrycode = col("countrycode", bpchar(3))
    val name        = col("name", text)
  }

  val schema =
    schema"""
      type Query {
        countries(offset: Int, limit: Int): PagedCountry!
      }
      type PagedCountry {
        items: [Country!]!
        hasMore: Boolean!
      }
      type Country {
        code: String!
        name: String!
        cities(offset: Int, limit: Int): PagedCity!
      }
      type PagedCity {
        items: [City!]!
        hasMore: Boolean!
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
            SqlObject("items"),
            CursorField("hasMore", CountryPaging.genHasMore, List("numCountries")),
            SqlField("numCountries", root.numCountries, hidden = true)
          )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings =
          List(
            SqlField("code", country.code, key = true),
            SqlField("name", country.name),
            SqlObject("cities")
          ),
      ),
      ObjectMapping(
        tpe = PagedCityType,
        fieldMappings =
          List(
            SqlField("code", country.code, key = true, hidden = true),
            SqlObject("items", Join(country.code, city.countrycode)),
            CursorField("hasMore", CityPaging.genHasMore, List("numCities")),
            SqlField("numCities", country.numCities, hidden = true)
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

  abstract class PagingConfig(key: String, countField: String, countAttr: String, orderTerm: Term[String]) {
    def setup(offset: Option[Int], limit: Option[Int]): Elab[Unit] =
      Elab.env(key -> new PagingInfo(offset, limit))

    def elabItems = Elab.envE[PagingInfo](key).flatMap(_.elabItems)
    def elabHasMore = Elab.envE[PagingInfo](key).flatMap(_.elabHasMore)

    def genHasMore(c : Cursor): Result[Boolean] =
      for {
        info <- c.envR[PagingInfo](key)
        c0   <- info.genHasMore(c)
      } yield c0

    case class PagingInfo(offset: Option[Int], limit: Option[Int]) {
      def elabItems: Elab[Unit] =
        Elab.transformChild { child =>
          FilterOrderByOffsetLimit(None, Some(List(OrderSelection(orderTerm))), offset, limit, child)
        }

      def elabHasMore: Elab[Unit] =
        Elab.addAttribute(countField, Count(Select("items", Select(countAttr))))

      def genHasMore(c: Cursor): Result[Boolean] =
        for {
          num <- c.fieldAs[Long](countField)
        } yield num > offset.getOrElse(0)+limit.getOrElse(num.toInt)
    }
  }

  object CountryPaging extends PagingConfig("countryPaging", "numCountries", "code", CountryType / "code")
  object CityPaging extends PagingConfig("cityPaging", "numCities", "name", CityType / "name")

  object OptIntValue {
    def unapply(v: Value): Option[Option[Int]] = v match {
      case IntValue(n) => Some(Some(n))
      case AbsentValue | NullValue => Some(None)
      case _ => None
    }
  }

  override val selectElaborator = SelectElaborator {
    case (QueryType, "countries", List(Binding("offset", OptIntValue(off)), Binding("limit", OptIntValue(lim)))) =>
      CountryPaging.setup(off, lim)

    case (PagedCountryType, "hasMore", Nil) =>
      CountryPaging.elabHasMore

    case (PagedCountryType, "items", Nil) =>
      CountryPaging.elabItems

    case (CountryType, "cities", List(Binding("offset", OptIntValue(off)), Binding("limit", OptIntValue(lim)))) =>
      CityPaging.setup(off, lim)

    case (PagedCityType, "hasMore", Nil) =>
      CityPaging.elabHasMore

    case (PagedCityType, "items", Nil) =>
      CityPaging.elabItems
  }
}
