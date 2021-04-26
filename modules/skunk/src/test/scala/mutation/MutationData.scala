// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package mutation

import _root_.skunk.codec.all._
import _root_.skunk.implicits._
import _root_.skunk.Session
import cats.data.IorT
import cats.effect.{ Bracket, Resource, Sync }
import cats.syntax.all._
import edu.gemini.grackle._
import edu.gemini.grackle.Path._
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query._
import edu.gemini.grackle.QueryCompiler._
import edu.gemini.grackle.skunk._
import edu.gemini.grackle.Value._
import grackle.test.SqlMutationSchema
import fs2.Stream

trait MutationSchema[F[_]] extends SkunkMapping[F] with SqlMutationSchema {

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
                  pool.use { s =>
                    s.prepare(sql"update city set population=$int4 where id=$int4".command).use { ps =>
                      IorT.right(ps.execute(pop ~ id).void).value // awkward
                    }
                  }
                }
              }
          }),
          SqlRoot("createCity", mutation = Mutation { (child, e) =>
            (e.get[String]("name"), e.get[String]("countryCode"), e.get[Int]("population")).tupled match {
              case None =>
                QueryInterpreter.mkErrorResult[(Query, Cursor.Env)](s"Implementation error: expected name, countryCode and population in $e.").pure[Stream[F,*]]
              case Some((name, cc, pop)) =>
                Stream.eval {
                  pool.use { s =>
                    val q = sql"""
                        INSERT INTO city (id, name, countrycode, district, population)
                        VALUES (nextval('city_id'), $varchar, ${bpchar(3)}, 'ignored', $int4)
                        RETURNING id
                      """.query(int4)
                    s.prepare(q).use { ps =>
                      ps.unique(name ~ cc ~ pop).map { id =>
                        (Unique(Eql(UniquePath(List("id")), Const(id)), child), e).rightIor
                      }
                    }
                  }
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
            // We already know the final form of the query so we can rewrite it here.
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

object MutationMapping extends SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F,Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with MutationMapping[F] {
      val ev = Sync[F]
    }

}
