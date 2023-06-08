// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.syntax.all._

import edu.gemini.grackle._
import syntax._
import Predicate._
import Query._
import QueryCompiler._
import Value._

trait SqlMutationMapping[F[_]] extends SqlTestMapping[F] {
  object country extends TableDef("country") {
    val code = col("code", bpchar(3))
    val name = col("name", text)
  }

  object city extends TableDef("city") {
    val id          = col("id", int4)
    val countrycode = col("countrycode", bpchar(2))
    val name        = col("name", text)
    val population  = col("population", int4)
  }

  val schema =
    schema"""
      type Query {
        city(id: Int!): City
      }
      type Mutation {
        updatePopulation(id: Int!, population: Int!): City
        createCity(
          name: String!
          countryCode: String!
          population: Int!
        ): City
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

  def updatePopulation(id: Int, population: Int): F[Unit]
  def createCity(name: String, countryCode: String, population: Int): F[Int]

  val QueryType    = schema.ref("Query")
  val MutationType = schema.ref("Mutation")
  val CountryType  = schema.ref("Country")
  val CityType     = schema.ref("City")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlObject("city"),
        ),
      ),
      ObjectMapping(
        tpe = MutationType,
        fieldMappings = List(
          RootEffect.computeQuery("updatePopulation")((query, _, env) =>
            (env.get[Int]("id"), env.get[Int]("population")).tupled match {
              case Some((id, pop)) =>
                updatePopulation(id, pop).as(Result(query))
              case None =>
                Result.internalError(s"Implementation error, expected id and population in $env.").pure[F].widen
            }
          ),
          RootEffect.computeQuery("createCity")((query, _, env) =>
            (env.get[String]("name"), env.get[String]("countryCode"), env.get[Int]("population")).tupled match {
              case Some((name, cc, pop)) =>
                query match {
                  case en@Environment(_, s@Select(_, _, child)) =>
                    createCity(name, cc, pop).map { id =>
                      Result(en.copy(child = s.copy(child = (Unique(Filter(Eql(CityType / "id", Const(id)), child))))))
                    }
                  case _ => Result.internalError(s"Implementation error: expected Environment node.").pure[F].widen
                }
              case None => Result.internalError(s"Implementation error: expected name, countryCode and population in $env.").pure[F].widen
            }
          )
        )
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
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("city", List(Binding("id", IntValue(id))), child) =>
        Select("city", Nil, Unique(Filter(Eql(CityType / "id", Const(id)), child))).success
    },
    MutationType -> {

      case Select("updatePopulation", List(Binding("id", IntValue(id)), Binding("population", IntValue(pop))), child) =>
        Environment(
          Cursor.Env("id" -> id, "population" -> pop),
          Select("updatePopulation", Nil,
            // We could also do this in the SqlRoot's mutation, and in fact would need to do so if
            // the mutation generated a new id. But for now it seems easiest to do it here.
            Unique(Filter(Eql(CityType / "id", Const(id)), child))
          )
        ).success

      case Select("createCity", List(Binding("name", StringValue(name)), Binding("countryCode", StringValue(code)), Binding("population", IntValue(pop))), child) =>
          Environment(
            Cursor.Env[Any]("name" -> name, "countryCode" -> code, "population" -> pop),
            Select("createCity", Nil, child)
          ).success

    }
  ))
}
