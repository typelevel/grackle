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

import cats.effect.Sync
import cats.implicits._
import fs2.concurrent.SignallingRef
import io.circe.Json

import grackle.{Cursor, Env, Query, Result}
import grackle.Query.EffectHandler
import grackle.QueryInterpreter.EffectErrorPolicy
import grackle.circe.CirceMapping
import grackle.syntax._

class TestCirceEffectHandlerErrorMapping[F[_]: Sync](
    ref: SignallingRef[F, Int],
    policy: EffectErrorPolicy)
    extends CirceMapping[F] {
  override def effectErrorPolicy: EffectErrorPolicy = policy

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
      queries
        .traverse {
          case (_, _) =>
            ref
              .update(_ + 1)
              .as(
                Result.failure[Cursor](s"value: $value")
              )
        }
        .map(_.sequence)
  }

  val nHandler = TestEffectHandler(42)
  val sHandler = TestEffectHandler("hi")

  val typeMappings = List(
    ObjectMapping(
      tpe = QueryType,
      fieldMappings = List(
        EffectField("n", nHandler, Nil),
        EffectField("s", sHandler, Nil)
      )
    )
  )

}

/**
 * As `TestCirceEffectHandlerErrorMapping`, but both fields are backed by a *single, shared*
 * handler. Because effects are batched by `(mapping, handler)`, this means both fields end up
 * in one batch and are passed to `runEffects` together. The handler accumulates a failure per
 * query using `parSequence`; note that combining with `sequence` here would short-circuit on
 * the first failure and drop the others.
 */
class TestCirceSharedEffectHandlerErrorMapping[F[_]: Sync](ref: SignallingRef[F, Int])
    extends CirceMapping[F] {
  val schema =
    schema"""
      type Query {
        n: Int!
        s: String!
      }
    """

  val QueryType = schema.ref("Query")

  val sharedHandler: EffectHandler[F] =
    new EffectHandler[F] {
      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        queries
          .traverse {
            case (query, _) =>
              val name = Query.rootName(query).map(_._1).getOrElse("?")
              ref
                .update(_ + 1)
                .as(
                  Result.failure[Cursor](s"value: $name")
                )
          }
          .map(_.parSequence)
    }

  val typeMappings = List(
    ObjectMapping(
      tpe = QueryType,
      fieldMappings = List(
        EffectField("n", sharedHandler, Nil),
        EffectField("s", sharedHandler, Nil)
      )
    )
  )
}

/**
 * Two top-level fields (`a`, `b`) share a single, succeeding handler, so they are batched
 * together and their continuations are completed as a *group* in the recursive `completeAll`
 * call. Each continuation contains nested effect fields (`x`, `y`) backed by a shared failing
 * handler which reports its position; the accumulated errors should appear in document order
 * (a/x, a/y, b/x, b/y).
 */
class TestCirceNestedEffectHandlerErrorMapping[F[_]: Sync] extends CirceMapping[F] {
  val schema =
    schema"""
      type Query {
        a: Child!
        b: Child!
      }
      type Child {
        x: Int!
        y: Int!
      }
    """

  val QueryType = schema.ref("Query")
  val ChildType = schema.ref("Child")

  val outerHandler: EffectHandler[F] =
    new EffectHandler[F] {
      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        queries
          .traverse {
            case (query, parentCursor) =>
              Query
                .childContext(parentCursor.context, query)
                .map(ctx => CirceCursor(ctx, Json.obj(), Some(parentCursor), Env.empty): Cursor)
          }
          .pure[F]
    }

  val innerHandler: EffectHandler[F] =
    new EffectHandler[F] {
      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        queries
          .map {
            case (query, cursor) =>
              val parent = cursor.context.path.headOption.getOrElse("?")
              val field = Query.rootName(query).map(_._1).getOrElse("?")
              Result.failure[Cursor](s"nested: $parent/$field")
          }
          .parSequence
          .pure[F]
    }

  val typeMappings = List(
    ObjectMapping(
      tpe = QueryType,
      fieldMappings = List(
        EffectField("a", outerHandler, Nil),
        EffectField("b", outerHandler, Nil)
      )
    ),
    ObjectMapping(
      tpe = ChildType,
      fieldMappings = List(
        EffectField("x", innerHandler, Nil),
        EffectField("y", innerHandler, Nil)
      )
    )
  )
}
