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

import cats.effect.Sync
import cats.implicits._
import fs2.concurrent.SignallingRef

import grackle.Cursor
import grackle.Query
import grackle.Query.EffectHandler
import grackle.Result
import grackle.circe.CirceMapping
import grackle.syntax._

class TestCirceEffectHandlerErrorMapping[F[_]: Sync](ref: SignallingRef[F, Int]) extends CirceMapping[F] {
  val schema =
    schema"""
      type Query {
        n: Int!
        s: String!
      }
    """

  val QueryType = schema.ref("Query")

  case class TestEffectHandler[A](value: A) extends EffectHandler[F] {
    def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
      queries.traverse { case (_, _) =>
        ref.update(_ + 1).as(
          Result.failure[Cursor](s"value: $value")
        )
      }.map(_.sequence)
  }

  val nHandler = TestEffectHandler(42)
  val sHandler = TestEffectHandler("hi")

  val typeMappings = List(
    ObjectMapping(
      tpe = QueryType,
      fieldMappings =
        List(
          EffectField("n", nHandler, Nil),
          EffectField("s", sHandler, Nil)
        )
    )
  )


}