// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package mutation

import _root_.doobie.{ Meta, Transactor }
import _root_.doobie.implicits._
import cats.effect.{ Bracket, Sync }
import cats.syntax.all._
import fs2.Stream

import edu.gemini.grackle._
import edu.gemini.grackle.doobie.DoobieMapping
import edu.gemini.grackle.doobie.DoobieMappingCompanion
import edu.gemini.grackle.doobie.DoobieMonitor
import edu.gemini.grackle.Path._
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query._
import edu.gemini.grackle.QueryCompiler._
import edu.gemini.grackle.Value._
import grackle.test.SqlMutationSchema

trait MutationSchema[F[_]] extends DoobieMapping[F] with SqlMutationSchema {

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
                Stream.eval {
                  sql"update city set population=$pop where id=$id"
                    .update
                    .run
                    .transact(transactor)
                    .as(().rightIor)
                }
              }
          }),
          SqlRoot("createCity", mutation = Mutation { (child, e) =>
            (e.get[String]("name"), e.get[String]("countryCode"), e.get[Int]("population")).tupled match {
              case None =>
                QueryInterpreter.mkErrorResult[(Query, Cursor.Env)](s"Implementation error: expected name, countryCode and population in $e.").pure[Stream[F,*]]
              case Some((name, cc, pop)) =>
                Stream.eval {
                  sql"""
                      INSERT INTO city (id, name, countrycode, district, population)
                      VALUES (nextval('city_id'), $name, $cc, 'ignored', $pop)
                      RETURNING id
                    """.query[Int]
                      .unique
                      .transact(transactor)
                      .map { id => (Unique(Eql(UniquePath(List("id")), Const(id)), child), e).rightIor }
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
        Select("city", Nil, Unique(Eql(UniquePath(List("id")), Const(id)), child)).rightIor
    },
    MutationType -> {

      case Select("updatePopulation", List(Binding("id", IntValue(id)), Binding("population", IntValue(pop))), child) =>
        Environment(
          Cursor.Env("id" -> id, "population" -> pop),
          Select("updatePopulation", Nil,
            // We could also do this in the SqlRoot's mutation, and in fact would need to do so if
            // the mutation generated a new id. But for now it seems easiest to do it here.
            Unique(Eql(UniquePath(List("id")), Const(id)), child)
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

object MutationMapping extends DoobieMappingCompanion {

  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F] =
    new DoobieMapping[F](transactor, monitor) with MutationMapping[F] {
      val ev = Sync[F]
    }

}
