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

import cats.syntax.all._

import grackle._
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

  case class UpdatePopulation(id: Int, population: Int)
  case class CreateCity(name: String, countryCode: String, population: Int)

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
          RootEffect.computeUnit("updatePopulation")(env =>
            env.getR[UpdatePopulation]("updatePopulation").traverse {
              case UpdatePopulation(id, pop) => updatePopulation(id, pop)
            }
          ),
          RootEffect.computeChild("createCity")((child, _, env) =>
            env.getR[CreateCity]("createCity").flatTraverse {
              case CreateCity(name, cc, pop) =>
                createCity(name, cc, pop).map { id =>
                  Unique(Filter(Eql(CityType / "id", Const(id)), child)).success
                }
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

  override val selectElaborator = SelectElaborator {
    case (QueryType, "city", List(Binding("id", IntValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CityType / "id", Const(id)), child)))

    case (MutationType, "updatePopulation", List(Binding("id", IntValue(id)), Binding("population", IntValue(pop)))) =>
      for {
        _ <- Elab.env("updatePopulation", UpdatePopulation(id, pop))
        _ <- Elab.transformChild(child => Unique(Filter(Eql(CityType / "id", Const(id)), child)))
      } yield ()

    case (MutationType, "createCity", List(Binding("name", StringValue(name)), Binding("countryCode", StringValue(code)), Binding("population", IntValue(pop)))) =>
      Elab.env("createCity", CreateCity(name, code, pop))
  }
}
