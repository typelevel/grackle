// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package subscription

import _root_.skunk.codec.all._
import _root_.skunk.implicits._
import _root_.skunk.Session
import cats.effect.{ Bracket, Resource, Sync }
import cats.syntax.all._
import edu.gemini.grackle._
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query._
import edu.gemini.grackle.QueryCompiler._
import edu.gemini.grackle.skunk._
import edu.gemini.grackle.Value._

trait SubscriptionMapping[F[_]] extends SkunkMapping[F] {

  val schema =
    Schema(
      """
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
    ).right.get

  class TableDef(name: String) {
    def col(colName: String, codec: Codec[_]): ColumnRef =
      ColumnRef(name, colName, codec)
  }

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

  implicit def ev: Bracket[F, Throwable]

  val QueryType        = schema.ref("Query")
  val SubscriptionType = schema.ref("Subscription")
  val CountryType      = schema.ref("Country")
  val CityType         = schema.ref("City")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlRoot("city"),
        ),
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings = List(
          SqlAttribute("code", country.code, key = true),
          SqlField("name",     country.name),
          SqlObject("cities",  Join(country.code, city.countrycode)),
        ),
      ),
      ObjectMapping(
        tpe = CityType,
        fieldMappings = List(
          SqlAttribute("id", city.id, key = true),
          SqlAttribute("countrycode", city.countrycode),
          SqlField("name", city.name),
          SqlField("population", city.population),
          SqlObject("country", Join(city.countrycode, country.code)),
        )
      ),
      ObjectMapping(
        tpe = SubscriptionType,
        fieldMappings = List(
          SqlRoot(
            fieldName = "channel",
            mutation  = Mutation { (q, e) =>
              for {
                s  <- fs2.Stream.resource(pool)
                id <- s.channel(id"city_channel").listen(256).map(_.value.toInt)
                qʹ  = Unique(Eql(AttrPath(List("id")), Const(id)), q)
              } yield Result((qʹ, e))
            }
          ),
        ),
      ),
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("city", List(Binding("id", IntValue(id))), child) =>
        Select("city", Nil, Unique(Eql(AttrPath(List("id")), Const(id)), child)).rightIor
    },
  ))
}

object SubscriptionMapping extends SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F,Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with SubscriptionMapping[F] {
      val ev = Sync[F]
    }

}
