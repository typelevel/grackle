// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package subscription

import scala.concurrent.duration._

import cats.effect._
import cats.implicits._
import fs2.concurrent.SignallingRef
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

import edu.gemini.grackle.{Cursor, Query, QueryCompiler, Mapping, Result, Schema, Value, ValueMapping}
import edu.gemini.grackle.syntax._

final class SubscriptionSuite extends CatsEffectSuite {

  def mapping(ref: SignallingRef[IO, Int]): Mapping[IO] =
    new ValueMapping[IO] {

      val schema: Schema =
        schema"""
          type Query {
            get: Int!
          }
          type Mutation {
            put(n: Int): Int!
          }
          type Subscription {
            watch: Int!
          }
        """

      val QueryType        = schema.ref("Query")
      val MutationType     = schema.ref("Mutation")
      val SubscriptionType = schema.ref("Subscription")

      val typeMappings: List[TypeMapping] =
        List(
          ObjectMapping(QueryType, List(
            RootEffect.computeCursor("get")((_, path, env) => ref.get.map(n => Result(valueCursor(path, env, n))))
          )),
          ObjectMapping(MutationType, List(
            RootEffect.computeCursor("put")( (_, path, env) =>
              env.get[Int]("n") match {
                case None    => Result.failure(s"Implementation error: `n: Int` not found in $env").pure[IO]
                case Some(n) => ref.set(n).map(_ => Result(valueCursor(path, env, n)))
              }
            )
          )),
          ObjectMapping(SubscriptionType, List(
            RootStream.computeCursor("watch")((_, path, env) =>
              ref.discrete.map(n => Result(valueCursor(path, env, n))))
          ))
        )

      override val selectElaborator: QueryCompiler.SelectElaborator =
        new QueryCompiler.SelectElaborator(Map(
          MutationType -> {
            case Query.Select("put", List(Query.Binding("n", Value.IntValue(n))), child) =>
              Result(Query.Environment(Cursor.Env("n" -> n), Query.Select("put", Nil, child)))
          }
        ))
    }

  test("sanity check get") {
    val prog: IO[Json] =
      for {
        ref <- SignallingRef[IO, Int](0)
        map  = mapping(ref)
        r1  <- map.compileAndRunOne("query { get }")
      } yield r1

    assertIO(prog,
      json"""
        {
          "data" : {
            "get" : 0
          }
        }
      """
    )
  }

  test("sanity check put") {
    val prog: IO[Json] =
      for {
        ref <- SignallingRef[IO, Int](0)
        map  = mapping(ref)
        r1  <- map.compileAndRunOne("mutation { put(n: 42) }")
      } yield r1

    assertIO(prog,
      json"""
        {
          "data" : {
            "put" : 42
          }
        }
      """
    )
  }

  test("sanity check (get, put, get)") {

    val prog: IO[(Json, Json, Json)] =
      for {
        ref <- SignallingRef[IO, Int](0)
        map  = mapping(ref)
        r0  <- map.compileAndRunOne("query { get }")
        r1  <- map.compileAndRunOne("mutation { put(n: 42) }")
        r2  <- map.compileAndRunOne("query { get }")
      } yield (r0, r1, r2)

    assertIO(prog, ((
      json"""
        {
          "data" : {
            "get" : 0
          }
        }
      """,
      json"""
        {
          "data" : {
            "put" : 42
          }
        }
      """,
      json"""
        {
          "data" : {
            "get" : 42
          }
        }
      """
    )))

  }

  test("subscription") {

    val prog: IO[List[Json]] =
      for {
        ref <- SignallingRef[IO, Int](0)
        map  = mapping(ref)
        fib <- map.compileAndRunAll("subscription { watch }").take(4).compile.toList.start
        _   <- IO.sleep(100.milli) // this is the best we can do for now; I will try to improve in a followup
        _   <- map.compileAndRunOne("mutation { put(n: 123) }")
        _   <- IO.sleep(100.milli)
        _   <- map.compileAndRunOne("mutation { put(n: 42) }")
        _   <- IO.sleep(100.milli)
        _   <- map.compileAndRunOne("mutation { put(n: 77) }")
        _   <- IO.sleep(100.milli)
        out <- fib.join
        res <- out.embedNever
      } yield res

    assertIO(prog, List(
      json"""
        {
          "data" : {
            "watch" : 0
          }
        }
      """,
      json"""
        {
          "data" : {
            "watch" : 123
          }
        }
      """,
      json"""
        {
          "data" : {
            "watch" : 42
          }
        }
      """,
      json"""
        {
          "data" : {
            "watch" : 77
          }
        }
      """,
    ))

  }
}
