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

import scala.concurrent.duration._

import cats.effect.IO
import cats.effect.testkit.TestControl
import io.circe.literal._
import munit.CatsEffectSuite

import grackle.Query.UntypedFragment
import grackle.QueryCompiler.IntrospectionLevel

/**
 * A parser that counts the documents it parses.
 */
final class CountingQueryParser(underlying: QueryParser) extends QueryParser {
  var count: Int = 0

  def parseText(text: String): Result[(List[UntypedOperation], List[UntypedFragment])] = {
    count += 1
    underlying.parseText(text)
  }

  def parseDocument(
      doc: Ast.Document): Result[(List[UntypedOperation], List[UntypedFragment])] =
    underlying.parseDocument(doc)
}

final class CachingQueryCompilerSuite extends CatsEffectSuite {

  def counting: (CountingQueryParser, QueryCompiler) = {
    val parser = new CountingQueryParser(CacheTestMapping.queryParser)
    val compiler =
      new QueryCompiler(parser, CacheTestMapping.schema, CacheTestMapping.compilerPhases)
    (parser, compiler)
  }

  /**
   * Runs `body` against a fresh counting parser and a caching compiler with the default store.
   */
  def withCache(body: (CountingQueryParser, CachingQueryCompiler[IO]) => IO[Unit]): IO[Unit] = {
    val (parser, compiler) = counting
    TestControl.executeEmbed(CachingQueryCompiler[IO](compiler).flatMap(body(parser, _)))
  }

  test("repeated documents parse once") {
    withCache { (parser, cached) =>
      for {
        first <- cached.compile("query { foo }")
        second <- cached.compile("query { foo }")
      } yield {
        assert(first.hasValue)
        assert(second.hasValue)
        assertEquals(parser.count, 1)
      }
    }
  }

  test("two different documents parse twice") {
    withCache { (parser, cached) =>
      for {
        _ <- cached.compile("query { foo }")
        _ <- cached.compile("query { bar }")
      } yield assertEquals(parser.count, 2)
    }
  }

  test("the same document with two different whitespace layouts parses twice") {
    withCache { (parser, cached) =>
      for {
        _ <- cached.compile("query { foo }")
        _ <- cached.compile("query {  foo }")
      } yield assertEquals(parser.count, 2)
    }
  }

  test("one cached document serves two different variable values") {
    val doc = "query ($n: Int!) { withArg(n: $n) }"
    withCache { (parser, cached) =>
      for {
        one <- cached.compile(doc, untypedVars = Some(json"""{ "n": 1 }"""))
        two <- cached.compile(doc, untypedVars = Some(json"""{ "n": 2 }"""))
      } yield {
        assertEquals(parser.count, 1)
        assert(one.hasValue)
        assert(two.hasValue)
        assertNotEquals(one.toOption.map(_.query), two.toOption.map(_.query))
      }
    }
  }

  test("one cached document does not leak the Env of the first request") {
    val doc = "query { secret }"
    withCache { (parser, cached) =>
      for {
        alice <- cached.compile(doc, env = Env("user" -> "alice"))
        bob <- cached.compile(doc, env = Env("user" -> "bob"))
        aliceAgain <- cached.compile(doc, env = Env("user" -> "alice"))
      } yield {
        assertEquals(parser.count, 1)
        assert(alice.hasValue, "the permitted user must compile")
        assert(!bob.hasValue, "the other user must not compile")
        assert(aliceAgain.hasValue, "a rejection must not poison the entry")
      }
    }
  }

  test("one cached document serves two different values of reportUnused") {
    val doc = "query ($unused: Int) { foo }"
    withCache { (parser, cached) =>
      for {
        reported <- cached.compile(doc, reportUnused = true)
        quiet <- cached.compile(doc, reportUnused = false)
      } yield {
        assertEquals(parser.count, 1)
        assert(reported.toProblems.exists(_.message.contains("is unused")))
        assert(!quiet.toProblems.exists(_.message.contains("is unused")))
      }
    }
  }

  test("a malformed document parses once and fails twice with the same problems") {
    val doc = "query { foo"
    withCache { (parser, cached) =>
      for {
        first <- cached.compile(doc)
        second <- cached.compile(doc)
      } yield {
        assertEquals(parser.count, 1)
        assert(!first.hasValue)
        assertEquals(first.toProblems.toList, second.toProblems.toList)
      }
    }
  }

  test("a cached result matches the result of the uncached compiler") {
    val (_, compiler) = counting
    val doc = "query ($n: Int!) { withArg(n: $n) }"
    val vars = json"""{ "n": 7 }"""
    TestControl.executeEmbed {
      for {
        cached <- CachingQueryCompiler[IO](compiler)
        _ <- cached.compile(doc, untypedVars = Some(vars))
        hot <- cached.compile(doc, untypedVars = Some(vars))
      } yield assertEquals(hot, compiler.compile(doc, untypedVars = Some(vars)))
    }
  }

  test("an entry which expires is parsed again") {
    withCache { (parser, cached) =>
      for {
        _ <- cached.compile("query { foo }")
        _ <- IO.sleep(61.minutes)
        _ <- cached.compile("query { foo }")
      } yield assertEquals(parser.count, 2)
    }
  }

  test("CachingQueryCompiler built with a caller-supplied store repeats a document once") {
    val (parser, compiler) = counting
    TestControl.executeEmbed {
      for {
        cache <- QueryCache[IO](maxSize = 4, ttl = 1.hour)
        cached = CachingQueryCompiler[IO](compiler, cache)
        first <- cached.compile("query { foo }")
        second <- cached.compile("query { foo }")
      } yield {
        assert(first.hasValue)
        assert(second.hasValue)
        assertEquals(parser.count, 1)
      }
    }
  }

  test("one cached document does not fix the introspection level") {
    val doc = "query { __schema { queryType { name } } }"
    withCache { (parser, cached) =>
      for {
        full <- cached.compile(doc, introspectionLevel = IntrospectionLevel.Full)
        restricted <- cached.compile(doc, introspectionLevel = IntrospectionLevel.TypenameOnly)
      } yield {
        assertEquals(parser.count, 1)
        assert(full.hasValue, "introspection must succeed when the level is Full")
        assert(!restricted.hasValue, "introspection must fail when the level is TypenameOnly")
        assert(restricted.toProblems.exists(_.message.contains("Introspection is disabled")))
      }
    }
  }
}
