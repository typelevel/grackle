// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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

package grackle.docs

import cats.effect._
import cats.implicits._
import fs2.concurrent.SignallingRef

import grackle._
import grackle.QueryCompiler._
import grackle.syntax._

object MutationSubscriptionMapping {

  // #ms_mapping
  // A single mutable cell of state shared by the query, mutation and subscription.
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
          // A query reads the current value.
          ObjectMapping(
            QueryType,
            List(
              RootEffect.computeCursor("get")((path, env) =>
                ref.get.map(n => Result(valueCursor(path, env, n))))
            )
          ),
          // A mutation runs an effect (here, a write) and returns a value to select over.
          ObjectMapping(
            MutationType,
            List(
              RootEffect.computeCursor("put")((path, env) =>
                env.get[Int]("n") match {
                  case None    => Result.failure(s"Implementation error: `n: Int` not found in $env").pure[IO]
                  case Some(n) => ref.set(n).map(_ => Result(valueCursor(path, env, n)))
                })
            )
          ),
          // A subscription is a stream of cursors, one per emitted value.
          ObjectMapping(
            SubscriptionType,
            List(
              RootStream.computeCursor("watch")((path, env) =>
                ref.discrete.map(n => Result(valueCursor(path, env, n))))
            )
          )
        )

      // Lift the `n` argument of `put` into the environment so the effect can read it.
      override val selectElaborator: SelectElaborator =
        SelectElaborator {
          case (MutationType, "put", List(Query.Binding("n", Value.IntValue(n)))) =>
            Elab.env("n" -> n)
        }
    }
  // #ms_mapping
}
