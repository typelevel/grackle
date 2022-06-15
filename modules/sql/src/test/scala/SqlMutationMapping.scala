// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package mutation

import cats.syntax.all._
import fs2.Stream

import edu.gemini.grackle._
import syntax._
import Path._
import Predicate._
import Query._
import QueryCompiler._
import Value._

import utils.SqlTestMapping

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
          SqlRoot("city"),
        ),
      ),
      ObjectMapping(
        tpe = MutationType,
        fieldMappings = List(
          SqlRoot("updatePopulation", mutation = Mutation.unit { (_, e) =>
            (e.get[Int]("id"), e.get[Int]("population")).tupled match {
              case None =>
                QueryInterpreter.mkErrorResult[Unit](s"Implementation error, expected id and population in $e.").pure[Stream[F,*]]
              case Some((id, pop)) =>
                Stream.eval(updatePopulation(id, pop)).map(_.rightIor)
              }
          }),
          SqlRoot("createCity", mutation = Mutation { (child, e) =>
            (e.get[String]("name"), e.get[String]("countryCode"), e.get[Int]("population")).tupled match {
              case None =>
                QueryInterpreter.mkErrorResult[(Query, Cursor.Env)](s"Implementation error: expected name, countryCode and population in $e.").pure[Stream[F,*]]
              case Some((name, cc, pop)) =>
                Stream.eval(createCity(name, cc, pop)).map { id =>
                  (Unique(Filter(Eql(UniquePath(List("id")), Const(id)), child)), e).rightIor
                }
            }
          }),
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
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("city", List(Binding("id", IntValue(id))), child) =>
        Select("city", Nil, Unique(Filter(Eql(UniquePath(List("id")), Const(id)), child))).rightIor
    },
    MutationType -> {

      case Select("updatePopulation", List(Binding("id", IntValue(id)), Binding("population", IntValue(pop))), child) =>
        Environment(
          Cursor.Env("id" -> id, "population" -> pop),
          Select("updatePopulation", Nil,
            // We could also do this in the SqlRoot's mutation, and in fact would need to do so if
            // the mutation generated a new id. But for now it seems easiest to do it here.
            Unique(Filter(Eql(UniquePath(List("id")), Const(id)), child))
          )
        ).rightIor

      case Select("createCity", List(Binding("name", StringValue(name)), Binding("countryCode", StringValue(code)), Binding("population", IntValue(pop))), child) =>
          Environment(
            Cursor.Env("name" -> name, "countryCode" -> code, "population" -> pop),
            Select("createCity", Nil, child)
          ).rightIor

    }
  ))
}
