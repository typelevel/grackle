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

import cats.effect.IO
import fs2.concurrent.SignallingRef
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

import grackle.QueryInterpreter.EffectErrorPolicy

final class CirceEffectHandlerErrorSuite extends CatsEffectSuite {

  // The `s` and `n` fields are backed by separate failing effect handlers, so they are
  // completed as two batches, in document order. The policy determines whether both batches
  // run (and both errors are reported) or completion stops at the first failed batch (with
  // the second handler's effect never run). The `Int` in the result is the number of handler
  // effects which actually ran.
  def runWithPolicy(policy: EffectErrorPolicy): IO[(Json, Int)] = {
    val query = """
      query {
        s,
        n
      }
    """

    for {
      ref <- SignallingRef[IO, Int](0)
      map = new TestCirceEffectHandlerErrorMapping(ref, policy)
      res <- map.compileAndRun(query)
      eff <- ref.get
    } yield (res, eff)
  }

  test("circe effect handler (accumulate)") {
    val expected = json"""
      {
        "errors" : [
          { "message": "value: hi", "path": ["s"] },
          { "message": "value: 42", "path": ["n"] }
        ],
        "data" : null
      }
    """

    assertIO(runWithPolicy(EffectErrorPolicy.Accumulate), (expected, 2))
  }

  test("circe effect handler (fail fast)") {
    val expected = json"""
      {
        "errors" : [
          { "message": "value: hi", "path": ["s"] }
        ],
        "data" : null
      }
    """

    assertIO(runWithPolicy(EffectErrorPolicy.FailFast), (expected, 1))
  }

  test("circe shared effect handler") {
    val query = """
      query {
        s,
        n
      }
    """

    val expected = json"""
      {
        "errors" : [
          { "message": "value: s" },
          { "message": "value: n" }
        ],
        "data" : null
      }
    """

    val prg: IO[(Json, Int)] =
      for {
        ref <- SignallingRef[IO, Int](0)
        map = new TestCirceSharedEffectHandlerErrorMapping(ref)
        res <- map.compileAndRun(query)
        eff <- ref.get
      } yield (res, eff)

    assertIO(prg, (expected, 2))
  }

  test("circe effect handler failure is a field error, sibling data is retained") {
    val query = """
      query {
        ping,
        viaEffect
      }
    """

    val expected = json"""
      {
        "errors" : [
          { "message": "boom", "path": ["viaEffect"] }
        ],
        "data" : {
          "ping" : "pong",
          "viaEffect" : null
        }
      }
    """

    val map = new TestCirceEffectHandlerSiblingMapping[IO]
    assertIO(map.compileAndRun(query), expected)
  }

  test("circe nested effect handler errors are accumulated in document order") {
    val query = """
      query {
        a {
          x
          y
        }
        b {
          x
          y
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          { "message": "nested: a/x" },
          { "message": "nested: a/y" },
          { "message": "nested: b/x" },
          { "message": "nested: b/y" }
        ],
        "data" : null
      }
    """

    val map = new TestCirceNestedEffectHandlerErrorMapping[IO]
    assertIO(map.compileAndRun(query), expected)
  }

  test("a failed continuation of a succeeding effect handler nulls its own position") {
    val query = """
      query {
        ping
        viaEffect {
          name
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          { "message": "boom", "path": ["viaEffect", "name"] }
        ],
        "data" : null
      }
    """

    val map = new TestCirceFailingContinuationMapping[IO]
    assertIO(map.compileAndRun(query), expected)
  }

  test("a null from a nested effect handler stops at the nearest nullable position") {
    val query = """
      query {
        ping
        child {
          viaEffect
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          { "message": "boom", "path": ["child", "viaEffect"] }
        ],
        "data" : {
          "ping" : "pong",
          "child" : null
        }
      }
    """

    val map = new TestCirceNestedNonNullEffectMapping[IO]
    assertIO(map.compileAndRun(query), expected)
  }

}
