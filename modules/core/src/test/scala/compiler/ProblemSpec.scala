// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext
import edu.gemini.grackle.Problem
import io.circe.syntax._
import io.circe.literal._

final class ProblemSpec extends CatsSuite {

  test("encoding (full)") {
    assert(
      Problem("foo", List(1 -> 2, 5 -> 6), List("bar", "baz")).asJson ==
      json"""
        {
          "message" : "foo",
          "locations" : [
            {
              "line" : 1,
              "col" : 2
            },
            {
              "line" : 5,
              "col" : 6
            }
          ],
          "path" : [
            "bar",
            "baz"
          ]
        }
      """
    )
  }

  test("encoding (no path)") {
    assert(
      Problem("foo", List(1 -> 2, 5 -> 6), Nil).asJson ==
      json"""
        {
          "message" : "foo",
          "locations" : [
            {
              "line" : 1,
              "col" : 2
            },
            {
              "line" : 5,
              "col" : 6
            }
          ]
        }
      """
    )
  }

  test("encoding (no locations)") {
    assert(
      Problem("foo", Nil, List("bar", "baz")).asJson ==
      json"""
        {
          "message" : "foo",
          "path" : [
            "bar",
            "baz"
          ]
        }
      """
    )
  }

  test("encoding (message only)") {
    assert(
      Problem("foo", Nil, Nil).asJson ==
      json"""
        {
          "message" : "foo"
        }
      """
    )
  }

}