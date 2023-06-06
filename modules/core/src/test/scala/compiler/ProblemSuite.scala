// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import io.circe.syntax._
import io.circe.JsonObject
import munit.CatsEffectSuite

import edu.gemini.grackle.syntax._
import edu.gemini.grackle.Problem

final class ProblemSuite extends CatsEffectSuite {

  test("encoding (full)") {
    assertEquals(
      Problem("foo", List(1 -> 2, 5 -> 6), List("bar", "baz")).asJson,
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
    assertEquals(
      Problem("foo", List(1 -> 2, 5 -> 6), Nil).asJson,
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
    assertEquals(
      Problem("foo", Nil, List("bar", "baz")).asJson,
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
    assertEquals(
      Problem("foo", Nil, Nil).asJson,
      json"""
        {
          "message" : "foo"
        }
      """
    )
  }

  test("toString (full)") {
    assertEquals(
      Problem("foo", List(1 -> 2, 5 -> 6), List("bar", "baz")).toString, "foo (at bar/baz: 1..2, 5..6)"
    )
  }

  test("toString (no path)") {
    assertEquals(
      Problem("foo", List(1 -> 2, 5 -> 6), Nil).toString, "foo (at 1..2, 5..6)"
    )
  }

  test("toString (no locations)") {
    assertEquals(
      Problem("foo", Nil, List("bar", "baz")).toString, "foo (at bar/baz)"
    )
  }

  test("toString (message only)") {
    assertEquals(
      Problem("foo", Nil, Nil).toString, "foo"
    )
  }

  test("extension") {
    val p = Problem("foo", extension = Some(JsonObject("bar" -> 42.asJson, "baz" -> List("a", "b").asJson)))
    assertEquals(
      p.asJson, json"""
        {
          "message" : "foo",
          "extension" : {
            "bar" : 42,
            "baz" : [
              "a",
              "b"
            ]
          }
        }
      """
    )
  }

}
