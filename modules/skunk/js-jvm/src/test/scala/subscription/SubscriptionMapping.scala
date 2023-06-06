// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.skunk.test.subscription

import cats.effect.{ Resource, Sync }
import skunk.Session
import skunk.codec.all._
import skunk.implicits._

import edu.gemini.grackle._
import syntax._
import Predicate._
import Query._
import QueryCompiler._
import skunk._
import Value._

trait SubscriptionMapping[F[_]] extends SkunkMapping[F] {

  val schema =
    schema"""
      type Query {
        city(id: Int!): City
      }
      type Subscription {
        channel: City!
      }
      type City {
        name: String!
        country: Country!
        population: Int!
      }
      type Country {
        name: String!
        cities: [City!]!
      }
    """

  object country extends TableDef("country") {
    val code = col("code", bpchar(3))
    val name = col("name", text)
  }

  object city extends TableDef("city") {
    val id          = col("id", int4)
    val countrycode = col("countrycode", bpchar(3))
    val name        = col("name", text)
    val population  = col("population", int4)
  }

  val QueryType        = schema.ref("Query")
  val SubscriptionType = schema.ref("Subscription")
  val CountryType      = schema.ref("Country")
  val CityType         = schema.ref("City")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlObject("city"),
        ),
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings = List(
          SqlField("code", country.code, key = true, hidden = true),
          SqlField("name",     country.name),
          SqlObject("cities",  Join(country.code, city.countrycode)),
        ),
      ),
      ObjectMapping(
        tpe = CityType,
        fieldMappings = List(
          SqlField("id", city.id, key = true, hidden = true),
          SqlField("countrycode", city.countrycode, hidden = true),
          SqlField("name", city.name),
          SqlField("population", city.population),
          SqlObject("country", Join(city.countrycode, country.code)),
        )
      ),
      ObjectMapping(
        tpe = SubscriptionType,
        fieldMappings = List(
          RootStream.computeQuery("channel")((query, _, _) =>
            for {
              s  <- fs2.Stream.resource(pool)
              id <- s.channel(id"city_channel").listen(256).map(_.value.toInt)
            } yield
              query match {
                case s@Select(_, _, child0) =>
                  Result(s.copy(child = Unique(Filter(Eql(CityType / "id", Const(id)), child0))))
                case _ => Result.internalError("Implementation error: expected Select")
              }
          )
        )
      )
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("city", List(Binding("id", IntValue(id))), child) =>
        Select("city", Nil, Unique(Filter(Eql(CityType / "id", Const(id)), child))).success
    },
  ))
}

object SubscriptionMapping extends SkunkMappingCompanion {
  def mkMapping[F[_]: Sync](pool: Resource[F,Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with SubscriptionMapping[F]
}
