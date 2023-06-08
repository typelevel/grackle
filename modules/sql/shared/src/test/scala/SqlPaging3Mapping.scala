// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.Order
import cats.implicits._

import edu.gemini.grackle._, syntax._
import Cursor.{Env, ListTransformCursor}
import Query.{Binding, Count, Empty, Environment, Group, Limit, Offset, OrderBy, OrderSelection, OrderSelections, Select, TransformCursor}
import QueryCompiler.SelectElaborator
import Value.IntValue

// Mapping illustrating paging in "has more" style: paged results can report
// whether there are more elements beyond the current sub list.
//
// In this implementation, if the query includes both `hasMore` and `items`
// then the compiled SQL query will overfetch by 1 item and compute `hasMore`
// based on the presence or absence of that extra item in the result. A
// `TransformCursor` is used to post-process the result items, removing any
// extra element before the result is returned to the client.
//
// If `hasMore` isn't included in the query then only the requested number of
// items will be fetched. If `items` isn't included in the query, then
// `hasMore` will be compiled to an SQL COUNT.
trait SqlPaging3Mapping[F[_]] extends SqlTestMapping[F] {
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
            CursorField("hasMore", genHasMore("country", "numCountries")),
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
          )
      ),
      ObjectMapping(
        tpe = PagedCityType,
        fieldMappings =
          List(
            SqlField("code", country.code, key = true, hidden = true),
            SqlObject("items", Join(country.code, city.countrycode)),
            CursorField("hasMore", genHasMore("city", "numCities")),
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

  def genHasMore(keyPrefix: String, countField: String)(c: Cursor): Result[Boolean] = {
    val limitKey = keyPrefix+"Limit"
    val lastKey = keyPrefix+"Last"
    val aliasKey = keyPrefix+"Alias"
    if(c.envContains(limitKey)) {
      for {
        limit <- c.envR[Int](limitKey)
        alias <- c.envR[Option[String]](aliasKey)
        items <- c.field("items", alias)
        size  <- items.listSize
      } yield limit < 0 || size > limit
    } else if(c.envContains(lastKey)) {
      for {
        last <- c.envR[Int](lastKey)
        num  <- c.fieldAs[Long](countField)
      } yield num > last
    } else
      Result.internalError("Result has unexpected shape")
  }

  def genItems(keyPrefix: String)(c: Cursor): Result[Cursor] = {
    val limitKey = keyPrefix+"Limit"
    for {
      limit <- c.envR[Int](limitKey)
      size  <- c.listSize
      elems <- c.asList(Seq)
    } yield {
      if(size <= limit) c
      else ListTransformCursor(c, size-1, elems.init)
    }
  }

  def transformChild[T: Order](query: Query, keyPrefix: String, orderTerm: Term[T], off: Int, lim: Int, hasHasMore: Boolean): Result[Query] =
    Query.mapFields(query) {
      case Select("items", Nil, child) =>
        def order(query: Query): Query =
          OrderBy(OrderSelections(List(OrderSelection(orderTerm))), query)

        def offset(query: Query): Query =
          if (off < 1) query
          else Offset(off, query)

        def limit(query: Query): Query =
          if (lim < 1) query
          else Limit(if (hasHasMore) lim+1 else lim, query)

        if(hasHasMore)
          Select("items", Nil, TransformCursor(genItems(keyPrefix), limit(offset(order(child))))).success
        else
          Select("items", Nil, limit(offset(order(child)))).success

      case other => other.success
    }

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("countries", List(Binding("offset", IntValue(off)), Binding("limit", IntValue(lim))), child) => {
        val hasItems = Query.hasField(child, "items")
        val hasHasMore = Query.hasField(child, "hasMore")
        if(hasItems) {
          val itemAlias = Query.fieldAlias(child, "items")
          transformChild[String](child, "country", CountryType / "code", off, lim, hasHasMore).map { child0 =>
            Select("countries", Nil, Environment(Env[Any]("countryLimit" -> lim, "countryAlias" -> itemAlias), child0))
          }
        } else
          Select("countries", Nil,
            Environment(Env("countryLast" -> (off+lim)),
              Group(List(
                Count("numCountries", Select("items", Nil, Select("code", Nil, Empty))),
                child
              ))
            )
          ).success
      }
    },
    CountryType -> {
      case Select("cities", List(Binding("offset", IntValue(off)), Binding("limit", IntValue(lim))), child) => {
        val hasItems = Query.hasField(child, "items")
        val hasHasMore = Query.hasField(child, "hasMore")
        if(hasItems) {
          val itemAlias = Query.fieldAlias(child, "items")
          transformChild[String](child, "city", CityType / "name", off, lim, hasHasMore).map { child0 =>
            Select("cities", Nil, Environment(Env[Any]("cityLimit" -> lim, "cityAlias" -> itemAlias), child0))
          }
        } else
          Select("cities", Nil,
            Environment(Env("cityLast" -> (off+lim)),
              Group(List(
                Count("numCities", Select("items", Nil, Select("id", Nil, Empty))),
                child
              ))
            )
          ).success
      }
    }
  ))
}
