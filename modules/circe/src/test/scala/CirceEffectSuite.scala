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

package grackle
package circetests

import cats.effect.IO
import fs2.concurrent.SignallingRef
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

final class CirceEffectSuite extends CatsEffectSuite {
  test("circe effect (nested)") {
    val query = """
      query {
        foo {
          s,
          n
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "foo" : {
            "s" : "hi",
            "n" : 42
          }
        }
      }
    """

    val prg: IO[(Json, Int)] =
      for {
        ref  <- SignallingRef[IO, Int](0)
        map  =  new TestCirceEffectMapping(ref)
        res  <- map.compileAndRun(query)
        eff  <- ref.get
      } yield (res, eff)

    assertIO(prg, (expected, 1))
  }

  test("circe effect (nested, aliased)") {
    val query = """
      query {
        quux:foo {
          s,
          n
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "quux" : {
            "s" : "hi",
            "n" : 42
          }
        }
      }
    """

    val prg: IO[(Json, Int)] =
      for {
        ref  <- SignallingRef[IO, Int](0)
        map  =  new TestCirceEffectMapping(ref)
        res  <- map.compileAndRun(query)
        eff  <- ref.get
      } yield (res, eff)

    assertIO(prg, (expected, 1))
  }

  test("circe effect (rooted)") {
    val query = """
      query {
        qux {
          s,
          n
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "qux" : {
            "s" : "hi",
            "n" : 42
          }
        }
      }
    """

    val prg: IO[(Json, Int)] =
      for {
        ref  <- SignallingRef[IO, Int](0)
        map  =  new TestCirceEffectMapping(ref)
        res  <- map.compileAndRun(query)
        eff  <- ref.get
      } yield (res, eff)

    assertIO(prg, (expected, 1))
  }

  test("circe effect (rooted, aliased)") {
    val query = """
      query {
        quux:qux {
          s,
          n
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "quux" : {
            "s" : "hi",
            "n" : 42
          }
        }
      }
    """

    val prg: IO[(Json, Int)] =
      for {
        ref  <- SignallingRef[IO, Int](0)
        map  =  new TestCirceEffectMapping(ref)
        res  <- map.compileAndRun(query)
        eff  <- ref.get
      } yield (res, eff)

    assertIO(prg, (expected, 1))
  }

  test("circe effect (multiple)") {
    val query = """
      query {
        foo {
          s,
          n
        }
        bar {
          s,
          n
        },
        baz {
          s,
          n
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "foo" : {
            "s" : "hi",
            "n" : 42
          },
          "bar" : {
            "s" : "ho",
            "n" : 42
          },
          "baz" : {
            "s" : "hee",
            "n" : 44
          }
        }
      }
    """

    val prg: IO[(Json, Int)] =
      for {
        ref  <- SignallingRef[IO, Int](0)
        map  =  new TestCirceEffectMapping(ref)
        res  <- map.compileAndRun(query)
        eff  <- ref.get
      } yield (res, eff)

    assertIO(prg, (expected, 3))
  }
}
