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

import scala.concurrent.duration.*

import cats.Monad
import cats.effect.kernel.{Clock, Ref, Temporal}
import cats.implicits.*
import io.circe.Json

import grackle.QueryCompiler.IntrospectionLevel
import grackle.QueryCompiler.IntrospectionLevel.Full

/**
 * Store of prepared GraphQL documents, keyed on the raw document text.
 *
 * A store belongs to one `QueryCompiler`, because a `PreparedDocument` depends on the schema of
 * the compiler that produced it. Do not share one store across two compilers with different
 * schemas.
 *
 * The store holds both parse successes and failures, as a `Result`. Repeat of malformed
 * documents therefore costs one lookup.
 */
trait QueryCache[F[_]] {
  def get(key: String): F[Option[Result[PreparedDocument]]]
  def put(key: String, value: Result[PreparedDocument]): F[Unit]
}

object QueryCache {

  private final case class Entry(doc: Result[PreparedDocument], expiry: FiniteDuration)

  /**
   * An in-memory store that holds up to `maxSize` documents (default 1024), each for `ttl`
   * (default one hour) after its last use (sliding-window).
   *
   * `maxSize` must be greater than zero.
   */
  def apply[F[_]: Temporal](
      maxSize: Int = 1024,
      ttl: FiniteDuration = 1.hour): F[QueryCache[F]] = {
    require(maxSize > 0, "maxSize must be greater than zero")

    Ref.of[F, Map[String, Entry]](Map.empty).map { ref =>
      new QueryCache[F] {

        def get(key: String): F[Option[Result[PreparedDocument]]] =
          Clock[F].monotonic.flatMap { now =>
            ref.modify { entries =>
              entries.get(key) match {
                case Some(Entry(doc, expiry)) if expiry > now =>
                  (entries.updated(key, Entry(doc, now + ttl)), Some(doc))
                case _ =>
                  (entries - key, None)
              }
            }
          }

        def put(key: String, value: Result[PreparedDocument]): F[Unit] =
          Clock[F].monotonic.flatMap { now =>
            ref.update { entries =>
              val room =
                if (entries.sizeIs < maxSize || entries.contains(key)) entries
                else evict(entries, now)
              room.updated(key, Entry(value, now + ttl))
            }
          }

        private def evict(
            entries: Map[String, Entry],
            now: FiniteDuration): Map[String, Entry] = {
          val (live, oldest) =
            entries.foldLeft((Map.empty[String, Entry], Option.empty[(String, Entry)])) {
              case ((live, oldest), kv @ (k, entry)) =>
                if (entry.expiry <= now) (live, oldest)
                else
                  (
                    live.updated(k, entry),
                    if (oldest.forall(_._2.expiry > entry.expiry)) Some(kv) else oldest)
            }

          if (live.sizeIs < entries.size) live
          else oldest.fold(live)(kv => live - kv._1)
        }
      }
    }
  }
}

/**
 * A `QueryCompiler` with a cache in front of the variable-free half of compilation.
 *
 * A repeat request with the same document text skips the parse and the document-level
 * validation.
 *
 * Build one instance for the life of the server, and one per `QueryCompiler`.
 */
final class CachingQueryCompiler[F[_]: Monad](compiler: QueryCompiler, cache: QueryCache[F]) {

  /**
   * Compiles the GraphQL document `text` to a query algebra term which can be directly
   * executed. Skips the parse and validation if the same `text` has been compiled before.
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
        case Some(prepared) =>
          prepared.pure[F]
        case None =>
          val prepared = compiler.prepare(text)
          cache.put(text, prepared).as(prepared)
      }
      .map(_.flatMap(
        compiler.compilePrepared(_, name, untypedVars, introspectionLevel, reportUnused, env)))
}

object CachingQueryCompiler {

  def apply[F[_]: Temporal](compiler: QueryCompiler): F[CachingQueryCompiler[F]] =
    QueryCache[F]().map(new CachingQueryCompiler(compiler, _))

  def apply[F[_]: Monad](
      compiler: QueryCompiler,
      cache: QueryCache[F]): CachingQueryCompiler[F] =
    new CachingQueryCompiler(compiler, cache)
}
