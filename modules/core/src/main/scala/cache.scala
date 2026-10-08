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

package grackle

import scala.collection.immutable.VectorMap

import cats.Monad
import cats.effect.kernel.{Concurrent, Ref}
import cats.implicits._
import io.circe.Json

import grackle.QueryCompiler.IntrospectionLevel
import grackle.QueryCompiler.IntrospectionLevel.Full
import grackle.QueryParser.ParsedDocument

/**
 * Store of parsed GraphQL documents, keyed on the raw document text.
 *
 * The store holds both parse successes and parse failures, as a `Result`.
 */
trait QueryCache[F[_]] {
  def get(key: String): F[Option[Result[ParsedDocument]]]
  def put(key: String, value: Result[ParsedDocument]): F[Unit]
}

object QueryCache {

  /**
   * A simple in-memory store that holds up to `maxSize` documents (default 1024). When the
   * store is full, a new document replaces the oldest document.
   *
   * `maxSize` must be greater than zero.
   */
  def apply[F[_]: Concurrent](maxSize: Int = 1024): F[QueryCache[F]] = {
    require(maxSize > 0, "maxSize must be greater than zero")

    Ref.of(VectorMap.empty[String, Result[ParsedDocument]]).map { ref =>
      new QueryCache[F] {

        def get(key: String): F[Option[Result[ParsedDocument]]] =
          ref.get.map(_.get(key))

        def put(key: String, value: Result[ParsedDocument]): F[Unit] =
          ref.update { entries =>
            val room =
              if (entries.sizeIs < maxSize || entries.contains(key)) entries
              else entries.tail
            room.updated(key, value)
          }
      }
    }
  }
}

/**
 * A `QueryCompiler` with a cache in front of the parser.
 *
 * A repeat request with the same document text skips the parse.
 *
 * Build one instance for the life of the server.
 */
final class CachingQueryCompiler[F[_]: Monad](compiler: QueryCompiler, cache: QueryCache[F]) {

  /**
   * Compiles the GraphQL document `text` to a query algebra term that can be directly executed.
   * Skips the parse if the same `text` was compiled before.
   */
  def compile(
      text: String,
      name: Option[String] = None,
      untypedVars: Option[Json] = None,
      introspectionLevel: IntrospectionLevel = Full,
      reportUnused: Boolean = true,
      env: Env = Env.empty): F[Result[Operation]] =
    cache
      .get(text)
      .flatMap {
        case Some(parsed) =>
          parsed.pure[F]
        case None =>
          val parsed = compiler.parser.parseText(text)
          // An internal error is a fault in the parser, not a property of the document.
          if (parsed.isInternalError) parsed.pure[F]
          else cache.put(text, parsed).as(parsed)
      }
      .map(_.flatMap(
        compiler.compileParsed(_, name, untypedVars, introspectionLevel, reportUnused, env)))
}

object CachingQueryCompiler {

  def apply[F[_]: Concurrent](compiler: QueryCompiler): F[CachingQueryCompiler[F]] =
    QueryCache[F]().map(new CachingQueryCompiler(compiler, _))

  def apply[F[_]: Monad](
      compiler: QueryCompiler,
      cache: QueryCache[F]): CachingQueryCompiler[F] =
    new CachingQueryCompiler(compiler, cache)
}
