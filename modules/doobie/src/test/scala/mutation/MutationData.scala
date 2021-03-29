// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package mutation

import _root_.doobie._
import _root_.doobie.implicits._
import cats.effect.{ Bracket, Sync }
import cats.syntax.all._
import edu.gemini.grackle._
import edu.gemini.grackle.doobie.DoobieMapping
import edu.gemini.grackle.doobie.DoobieMappingCompanion
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query._
import edu.gemini.grackle.QueryCompiler._
import edu.gemini.grackle.Value._
import edu.gemini.grackle.doobie.DoobieMonitor

trait MutationSchema[F[_]] extends DoobieMapping[F] {

  class TableDef(name: String) {
    def col(colName: String, codec: Codec[_]): ColumnRef =
      ColumnRef(name, colName, codec)
  }

  object country extends TableDef("country") {
    val code = col("code", Meta[String])
    val name = col("name", Meta[String])
  }

  object city extends TableDef("city") {
    val id          = col("id", Meta[Int])
    val countrycode = col("countrycode", Meta[String])
    val name        = col("name", Meta[String])
    val population  = col("population", Meta[Int])
  }

}

trait MutationMapping[F[_]] extends MutationSchema[F] {

  implicit def ev: Bracket[F, Throwable]

  val schema =
    Schema(
      """
        type Query {
          city(id: Int!): City
        }
        type Mutation {
          updatePopulation(id: Int!, population: Int!): City
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
                QueryInterpreter.mkErrorResult(s"Implementation error, expected id and population in $e.").pure[F]
              case Some((id, pop)) =>
                sql"update city set population=$pop where id=$id"
                  .update
                  .run
                  .transact(transactor)
                  .as(().rightIor)
              }
          }),
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
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("city", List(Binding("id", IntValue(id))), child) =>
        Select("city", Nil, Unique(Eql(AttrPath(List("id")), Const(id)), child)).rightIor
    },
    MutationType -> {
      case Select("updatePopulation", List(Binding("id", IntValue(id)), Binding("population", IntValue(pop))), child) =>
        Environment(
          Cursor.Env("id" -> id, "population" -> pop),
          Select("updatePopulation", Nil,
            // We could also do this in the SqlRoot's mutation, and in fact would need to do so if
            // the mutation generated a new id. But for now it seems easiest to do it here.
            Unique(Eql(AttrPath(List("id")), Const(id)), child)
          )
        ).rightIor
    }
  ))
}

object MutationMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with MutationMapping[F] {
      val ev = Sync[F]
    }

}
