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

package subscription

import scala.concurrent.duration._

import cats.effect._
import cats.implicits._
import fs2.concurrent.SignallingRef
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import QueryCompiler._

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

      val typeMappings =
        List(
          ObjectMapping(QueryType, List(
            RootEffect.computeCursor("get")((path, env) => ref.get.map(n => Result(valueCursor(path, env, n))))
          )),
          ObjectMapping(MutationType, List(
            RootEffect.computeCursor("put")( (path, env) =>
              env.get[Int]("n") match {
                case None    => Result.failure(s"Implementation error: `n: Int` not found in $env").pure[IO]
                case Some(n) => ref.set(n).map(_ => Result(valueCursor(path, env, n)))
              }
            )
          )),
          ObjectMapping(SubscriptionType, List(
            RootStream.computeCursor("watch")((path, env) =>
              ref.discrete.map(n => Result(valueCursor(path, env, n))))
          ))
        )

      override val selectElaborator: SelectElaborator =
        SelectElaborator {
          case (MutationType, "put", List(Query.Binding("n", Value.IntValue(n)))) =>
            Elab.env("n" -> n)
        }
    }

  test("sanity check get") {
    val prog: IO[Json] =
      for {
        ref <- SignallingRef[IO, Int](0)
        map  = mapping(ref)
        r1  <- map.compileAndRun("query { get }")
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
        r1  <- map.compileAndRun("mutation { put(n: 42) }")
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
        r0  <- map.compileAndRun("query { get }")
        r1  <- map.compileAndRun("mutation { put(n: 42) }")
        r2  <- map.compileAndRun("query { get }")
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

  test("serial execution") {
    val mutation =
      """
        mutation {
          one:put(n: 1)
          two:put(n: 2)
          three:put(n: 3)
        }
      """

    val prog: IO[Json] =
      for {
        ref <- SignallingRef[IO, Int](0)
        map  = mapping(ref)
        r  <- map.compileAndRun(mutation)
      } yield r

    assertIO(prog,
      json"""
        {
          "data" : {
            "one" : 1,
            "two" : 2,
            "three" : 3
          }
        }
      """
    )
  }

  test("subscription") {

    val prog: IO[List[Json]] =
      for {
        ref <- SignallingRef[IO, Int](0)
        map  = mapping(ref)
        fib <- map.compileAndRunSubscription("subscription { watch }").take(4).compile.toList.start
        _   <- IO.sleep(100.milli) // this is the best we can do for now; I will try to improve in a followup
        _   <- map.compileAndRun("mutation { put(n: 123) }")
        _   <- IO.sleep(100.milli)
        _   <- map.compileAndRun("mutation { put(n: 42) }")
        _   <- IO.sleep(100.milli)
        _   <- map.compileAndRun("mutation { put(n: 77) }")
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
