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

package conformance

import io.circe.{ACursor, Json, JsonObject}
import io.circe.literal._
import io.circe.syntax._

import grackle.Problem

/**
 * Conformance test cases for section 7, Response.
 *
 * Both examples of this section omit the definition of the variable `$episode`, so neither one
 * is a valid request as written. Each test case adds the definition and supplies a value. A
 * comment marks the addition.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Response
 */
final class ResponseSuite extends ConformanceSuite {

  // 7.1.4 Response Position
  // https://spec.graphql.org/September2025/#sec-Response-Position

  // The specification names four response paths for this request: the hero's name at
  // ["hero", "name"], the list of friends at ["hero", "friends"], the first friend at
  // ["hero", "friends", 0] and that friend's name at ["hero", "friends", 0, "name"].
  test("one field execution can produce more than one response position") {
    val response =
      ResponseMappings
        .NullableName
        .compileAndRun(
          """
            query ($episode: Episode!) {
              hero(episode: $episode) {
                name
                friends {
                  name
                }
              }
            }
          """,
          untypedVars = Some(json"""{"episode": "EMPIRE"}""")
        )

    assertIO(
      response.map(r =>
        List(
          position(r, "hero", "name"),
          position(r, "hero", "friends"),
          position(r, "hero", "friends", 0),
          position(r, "hero", "friends", 0, "name")
        )),
      List(
        Some(json""""Luke Skywalker""""),
        Some(json"""[{ "name": "Leia Organa" }]"""),
        Some(json"""{ "name": "Leia Organa" }"""),
        Some(json""""Leia Organa"""")
      )
    )
  }

  // 7.1.6 Errors
  // https://spec.graphql.org/September2025/#sec-Request-Error-Result

  // Grackle discards the whole `data` entry when a field raises an error, and it attaches
  // neither `path` nor `locations` to the error. The response is
  // `{"errors": [{"message": "..."}], "data": null}`.
  /**
   * The request of section 7.1.6, which the specification runs against two schemas.
   */
  private val heroFriendsDoc = """
    query ($episode: Episode!) {
      hero(episode: $episode) {
        name
        heroFriends: friends {
          id
          name
        }
      }
    }
  """

  yields(
    "an error carries the response path of the position which raised it".fail,
    ResponseMappings.NullableName,
    json"""{"episode": "NEWHOPE"}""")(heroFriendsDoc)(json"""
    {
      "errors": [
        {
          "message": "Name for character with ID 1002 could not be fetched.",
          "locations": [{ "line": 6, "column": 7 }],
          "path": ["hero", "heroFriends", 1, "name"]
        }
      ],
      "data": {
        "hero": {
          "name": "R2-D2",
          "heroFriends": [
            {
              "id": "1000",
              "name": "Luke Skywalker"
            },
            {
              "id": "1002",
              "name": null
            },
            {
              "id": "1003",
              "name": "Leia Organa"
            }
          ]
        }
      }
    }
  """)

  // The same request against a schema whose `name` field is non-null. The null bubbles up to the
  // nearest nullable position, which is the entry of the `heroFriends` list.
  yields(
    "a null from an error bubbles up to the nearest nullable position".fail,
    ResponseMappings.NonNullName,
    json"""{"episode": "NEWHOPE"}""")(heroFriendsDoc)(json"""
    {
      "errors": [
        {
          "message": "Name for character with ID 1002 could not be fetched.",
          "locations": [{ "line": 6, "column": 7 }],
          "path": ["hero", "heroFriends", 1, "name"]
        }
      ],
      "data": {
        "hero": {
          "name": "R2-D2",
          "heroFriends": [
            {
              "id": "1000",
              "name": "Luke Skywalker"
            },
            null,
            {
              "id": "1003",
              "name": "Leia Organa"
            }
          ]
        }
      }
    }
  """)

  // Section 7.1.6 states that each location is a map with the keys `line` and `column`. Grackle
  // writes the key `col`. This test case isolates that difference from the two above.
  test("an error location uses the keys line and column".fail) {
    assertEquals(
      Problem(
        "Name for character with ID 1002 could not be fetched.",
        List(6 -> 7),
        Nil).asJson,
      json"""
        {
          "message": "Name for character with ID 1002 could not be fetched.",
          "locations": [{ "line": 6, "column": 7 }]
        }
      """
    )
  }

  // The specification states this error with `locations` and `path`. The two test cases above
  // cover those two entries, so this test case states the `extensions` entry only.
  test("an error can carry an extensions map") {
    assertEquals(
      Problem(ResponseMappings.unfetchableMessage, Nil, Nil, Some(errorExtensions)).asJson,
      json"""
        {
          "message": "Name for character with ID 1002 could not be fetched.",
          "extensions": {
            "code": "CAN_NOT_FETCH_BY_ID",
            "timestamp": "Fri Feb 9 14:33:09 UTC 2018"
          }
        }
      """
    )
  }

  // The counter-example of this subject writes `code` and `timestamp` at the top level of the
  // error. Grackle writes them inside `extensions`, so the error holds no other entry.
  test("an error carries no entry of its own beside message, locations, path and extensions") {
    assertEquals(
      Problem(ResponseMappings.unfetchableMessage, Nil, Nil, Some(errorExtensions))
        .asJson
        .hcursor
        .keys
        .map(_.toList),
      Some(List("message", "extensions"))
    )
  }

  /**
   * The `extensions` map which section 7.1.6 states.
   */
  private val errorExtensions: JsonObject =
    JsonObject(
      "code" -> json""""CAN_NOT_FETCH_BY_ID"""",
      "timestamp" -> json""""Fri Feb 9 14:33:09 UTC 2018""""
    )

  /**
   * The value at `path` in the `data` entry of `response`.
   *
   * Section 7.1.4 defines a response path as a list of path segments. A segment which names a
   * field is a string, and a segment which indexes a list is an integer.
   */
  private def position(response: Json, path: Any*): Option[Json] =
    path
      .foldLeft(response.hcursor.downField("data"): ACursor) {
        case (cursor, name: String) => cursor.downField(name)
        case (cursor, index: Int) => cursor.downN(index)
        case (_, segment) => fail(s"'$segment' is not a response path segment")
      }
      .focus
}
