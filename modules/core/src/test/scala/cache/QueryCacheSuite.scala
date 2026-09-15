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

import cats.effect.IO
import cats.implicits.*
import compiler.TestMapping
import munit.CatsEffectSuite

import grackle.QueryCompiler.*
import grackle.QueryParser.ParsedDocument
import grackle.syntax.*

final class QueryCacheSuite extends CatsEffectSuite {

  def parse(text: String): Result[ParsedDocument] =
    CacheTestMapping.queryParser.parseText(text)

  val docA: Result[ParsedDocument] = parse("query { foo }")
  val docB: Result[ParsedDocument] = parse("query { bar }")
  val docC: Result[ParsedDocument] = parse("query { baz }")
  val malformed: Result[ParsedDocument] = parse("query { foo")

  test("stored document comes back") {
    for {
      cache <- QueryCache[IO](maxSize = 4)
      _ <- cache.put("a", docA)
      hit <- cache.get("a")
    } yield assertEquals(hit, Option(docA))
  }

  test("stored parse failure comes back") {
    assert(malformed.isFailure)
    for {
      cache <- QueryCache[IO](maxSize = 4)
      _ <- cache.put("m", malformed)
      hit <- cache.get("m")
    } yield assertEquals(hit, Option(malformed))
  }

  test("absent document misses") {
    for {
      cache <- QueryCache[IO](maxSize = 4)
      miss <- cache.get("a")
    } yield assertEquals(miss, None)
  }

  test("write past the size limit evicts the oldest entry") {
    for {
      cache <- QueryCache[IO](maxSize = 2)
      _ <- cache.put("a", docA)
      _ <- cache.put("b", docB)
      _ <- cache.put("c", docC)
      a <- cache.get("a")
      b <- cache.get("b")
      c <- cache.get("c")
    } yield {
      assertEquals(a, None, "the oldest entry must go")
      assertEquals(b, Option(docB))
      assertEquals(c, Option(docC), "the new entry must be present")
    }
  }

  test("read does not change which entry is the oldest") {
    for {
      cache <- QueryCache[IO](maxSize = 2)
      _ <- cache.put("a", docA)
      _ <- cache.put("b", docB)
      _ <- cache.get("a")
      _ <- cache.put("c", docC)
      a <- cache.get("a")
      b <- cache.get("b")
    } yield {
      assertEquals(a, None, "a read must not keep an entry")
      assertEquals(b, Option(docB))
    }
  }

  test("write to a present key replaces the value and does not evict another entry") {
    for {
      cache <- QueryCache[IO](maxSize = 2)
      _ <- cache.put("a", docA)
      _ <- cache.put("b", docB)
      _ <- cache.put("b", docC)
      a <- cache.get("a")
      b <- cache.get("b")
    } yield {
      assertEquals(a, Option(docA))
      assertEquals(b, Option(docC))
    }
  }

  test("write to a present key does not change which entry is the oldest") {
    for {
      cache <- QueryCache[IO](maxSize = 2)
      _ <- cache.put("a", docA)
      _ <- cache.put("b", docB)
      _ <- cache.put("a", docC)
      _ <- cache.put("c", docC)
      a <- cache.get("a")
      b <- cache.get("b")
    } yield {
      assertEquals(a, None, "a write must not move an entry")
      assertEquals(b, Option(docB))
    }
  }

  test("store with many more writes than its size limit keeps the newest entries") {
    val keys = (0 until 1000).toList.map(i => s"k$i")
    for {
      cache <- QueryCache[IO](maxSize = 10)
      _ <- keys.traverse_(cache.put(_, docA))
      hits <- keys.traverse(cache.get(_))
    } yield assertEquals(hits.map(_.isDefined), List.fill(990)(false) ++ List.fill(10)(true))
  }
}

object CacheTestMapping extends TestMapping {
  val schema =
    schema"""
      type Query {
        foo: Int
        bar: Int
        baz: Int
        withArg(n: Int!): Int
        secret: Int
      }
    """

  val QueryType = schema.ref("Query")

  override val selectElaborator = SelectElaborator {
    case (QueryType, "secret", Nil) =>
      Elab.env[String]("user").flatMap {
        case Some("alice") => Elab.unit
        case other => Elab.liftR(Result.failure(s"Not permitted for $other"))
      }

    case (QueryType, "withArg", List(Query.Binding("n", Value.IntValue(n)))) =>
      Elab.env("n" -> n)
  }
}
