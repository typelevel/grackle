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

package compiler

import io.circe.syntax._
import io.circe.JsonObject
import io.circe.literal._
import munit.CatsEffectSuite

import grackle.Problem

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
    val p = Problem("foo", extensions = Some(JsonObject("bar" -> 42.asJson, "baz" -> List("a", "b").asJson)))
    assertEquals(
      p.asJson, json"""
        {
          "message" : "foo",
          "extensions" : {
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
