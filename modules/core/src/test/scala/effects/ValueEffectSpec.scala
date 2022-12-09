// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.tests.CatsSuite
import fs2.concurrent.SignallingRef
import io.circe.Json

import edu.gemini.grackle.syntax._

final class ValueEffectSpec extends CatsSuite {
  test("value effect") {
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
        map  =  new ValueEffectMapping(ref)
        res  <- map.compileAndRun(query)
        eff  <- ref.get
      } yield (res, eff)

    val (res, eff) = prg.unsafeRunSync()
    //println(res)
    //println(eff)

    assert(res == expected)
    assert(eff == 1)
  }

  test("value effect, aliased") {
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
        map  =  new ValueEffectMapping(ref)
        res  <- map.compileAndRun(query)
        eff  <- ref.get
      } yield (res, eff)

    val (res, eff) = prg.unsafeRunSync()
    //println(res)
    //println(eff)

    assert(res == expected)
    assert(eff == 1)
  }
}
