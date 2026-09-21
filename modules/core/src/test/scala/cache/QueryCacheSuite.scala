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

import cats.effect.IO
import cats.effect.testkit.TestControl
import compiler.TestMapping
import munit.CatsEffectSuite

import grackle.QueryCompiler.*
import grackle.syntax.*

final class QueryCacheSuite extends CatsEffectSuite {

  val docA: Result[PreparedDocument] = CacheTestMapping.compiler.prepare("query { foo }")
  val docB: Result[PreparedDocument] = CacheTestMapping.compiler.prepare("query { bar }")
  val docC: Result[PreparedDocument] = CacheTestMapping.compiler.prepare("query { baz }")

  test("stored document comes back") {
    TestControl.executeEmbed {
      for {
        cache <- QueryCache[IO](maxSize = 4, ttl = 1.hour)
        _ <- cache.put("a", docA)
        hit <- cache.get("a")
      } yield assertEquals(hit, Option(docA))
    }
  }

  test("absent document misses") {
    TestControl.executeEmbed {
      for {
        cache <- QueryCache[IO](maxSize = 4, ttl = 1.hour)
        miss <- cache.get("a")
      } yield assertEquals(miss, None)
    }
  }

  test("entry which is never read expires after the time to live") {
    TestControl.executeEmbed {
      for {
        cache <- QueryCache[IO](maxSize = 4, ttl = 1.hour)
        _ <- cache.put("a", docA)
        _ <- IO.sleep(61.minutes)
        miss <- cache.get("a")
      } yield assertEquals(miss, None)
    }
  }

  test("read refreshes the expiry") {
    TestControl.executeEmbed {
      for {
        cache <- QueryCache[IO](maxSize = 4, ttl = 1.hour)
        _ <- cache.put("a", docA)
        _ <- IO.sleep(30.minutes)
        first <- cache.get("a")
        _ <- IO.sleep(31.minutes)
        second <- cache.get("a")
      } yield {
        assert(first.isDefined)
        assert(second.isDefined)
      }
    }
  }

  test("write past the size limit evicts the least recently used entry") {
    TestControl.executeEmbed {
      for {
        cache <- QueryCache[IO](maxSize = 2, ttl = 1.hour)
        _ <- cache.put("a", docA)
        _ <- IO.sleep(1.minute)
        _ <- cache.put("b", docB)
        _ <- IO.sleep(1.minute)
        _ <- cache.get("a") // "a" is now newer than "b"
        _ <- IO.sleep(1.minute)
        _ <- cache.put("c", docC)
        a <- cache.get("a")
        b <- cache.get("b")
        c <- cache.get("c")
      } yield {
        assertEquals(a, Option(docA), "the entry which was read must survive")
        assertEquals(b, None, "the least recently used entry must go")
        assertEquals(c, Option(docC), "the new entry must be present")
      }
    }
  }

  test("write to a present key does not evict another entry") {
    TestControl.executeEmbed {
      for {
        cache <- QueryCache[IO](maxSize = 2, ttl = 1.hour)
        _ <- cache.put("a", docA)
        _ <- IO.sleep(1.minute)
        _ <- cache.put("b", docB)
        _ <- IO.sleep(1.minute)
        _ <- cache.put("b", docB)
        a <- cache.get("a")
        b <- cache.get("b")
      } yield {
        assertEquals(a, Option(docA))
        assertEquals(b, Option(docB))
      }
    }
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
