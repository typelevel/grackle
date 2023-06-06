// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package circetests

import cats.effect.IO
import fs2.concurrent.SignallingRef
import io.circe.Json
import munit.CatsEffectSuite

import edu.gemini.grackle.syntax._

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
