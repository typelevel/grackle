// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.Order
import cats.implicits._

import edu.gemini.grackle._, syntax._
import Cursor.Env
import Query.{Binding, Count, Empty, Environment, Group, Limit, Offset, OrderBy, OrderSelection, OrderSelections, Select}
import QueryCompiler.SelectElaborator
import Value.IntValue

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
            CursorField("hasMore", genHasMore("countryLast", "numCountries"), List("numCountries")),
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
            CursorField("hasMore", genHasMore("cityLast", "numCities"), List("numCities")),
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

  def genHasMore(lastKey: String, numField: String)(c: Cursor): Result[Boolean] =
    for {
      last <- c.envR[Int](lastKey)
      num  <- c.fieldAs[Long](numField)
    } yield num > last

  def transformChild[T: Order](query: Query, orderTerm: Term[T], off: Int, lim: Int): Result[Query] =
    Query.mapFields(query) {
      case Select("items", Nil, child) =>
        def order(query: Query): Query =
          OrderBy(OrderSelections(List(OrderSelection(orderTerm))), query)

        def offset(query: Query): Query =
          if (off < 1) query
          else Offset(off, query)

        def limit(query: Query): Query =
          if (lim < 1) query
          else Limit(lim, query)

        Select("items", Nil, limit(offset(order(child)))).rightIor

      case other => other.rightIor
    }

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("countries", List(Binding("offset", IntValue(off)), Binding("limit", IntValue(lim))), child) => {
        transformChild[String](child, CountryType / "code", off, lim).map { child0 =>
          Select("countries", Nil, Environment(Env("countryLast" -> (off+lim)), child0))
        }
      }
    },
    PagedCountryType -> {
      case s@Select("hasMore", Nil, Empty) =>
        Group(List(
          s,
          Count("numCountries", Select("items", Nil, Select("code", Nil, Empty)))
        )).rightIor
    },
    CountryType -> {
      case Select("cities", List(Binding("offset", IntValue(off)), Binding("limit", IntValue(lim))), child) => {
        transformChild[String](child, CityType / "name", off, lim).map { child0 =>
          Select("cities", Nil, Environment(Env("cityLast" -> (off+lim)), child0))
        }
      }
    },
    PagedCityType -> {
      case s@Select("hasMore", Nil, Empty) =>
        Group(List(
          s,
          Count("numCities", Select("items", Nil, Select("id", Nil, Empty)))
        )).rightIor
    }
  ))
}
