// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package circetests

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.tests.CatsSuite
import edu.gemini.grackle.syntax._

final class CirceEffectSpec extends CatsSuite {
  test("circe effect") {
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

    val res = new TestCirceEffectMapping[IO].compileAndRun(query).unsafeRunSync()
    //println(res)

    assert(res == expected)
  }
}
