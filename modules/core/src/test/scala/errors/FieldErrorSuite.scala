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

package errors

import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

/**
 * Tests for the handling of field errors.
 *
 * A field error does not discard the response. The failed position completes as null, the null
 * bubbles up while the enclosing position is non-null, and the error carries the response path
 * of its own position.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Handling-Field-Errors
 */
final class FieldErrorSuite extends CatsEffectSuite {
  import FieldErrorMappings._

  private val query = """
    query {
      ping
      items {
        id
        name
      }
    }
  """

  /**
   * The response for `query`, with `data` as its data entry.
   *
   * Every case of `query` reports the same single error at the same position, so only the data
   * entry tells the cases apart.
   */
  private def expected(data: Json): Json =
    json"""
      {
        "errors": [
          {
            "message": $message,
            "path": ["items", 1, "name"]
          }
        ],
        "data": $data
      }
    """

  test("a field error keeps the data of the positions which succeeded") {
    val data = json"""
      {
        "ping": "pong",
        "items": [
          { "id": "1", "name": "one" },
          { "id": "2", "name": null },
          { "id": "3", "name": "three" }
        ]
      }
    """

    assertIO(NullableName.compileAndRun(query), expected(data))
  }

  test("a null from a non-null position bubbles up to the nearest nullable position") {
    val data = json"""
      {
        "ping": "pong",
        "items": [
          { "id": "1", "name": "one" },
          null,
          { "id": "3", "name": "three" }
        ]
      }
    """

    assertIO(NonNullName.compileAndRun(query), expected(data))
  }

  test("a null bubbles up to the data entry when no enclosing position is nullable") {
    assertIO(NonNullThroughout.compileAndRun(query), expected(Json.Null))
  }

  test("a warning at a position carries the response path of that position") {
    val query = """
      query {
        items {
          name
        }
      }
    """

    val expected = json"""
      {
        "errors": [
          {
            "message": $message,
            "path": ["items", 1, "name"]
          }
        ],
        "data": {
          "items": [
            { "name": "one" },
            { "name": "two" },
            { "name": "three" }
          ]
        }
      }
    """

    assertIO(WarningName.compileAndRun(query), expected)
  }

  test("a failed count field keeps the data of the positions which succeeded") {
    val query = """
      query {
        ping
        tagCount
      }
    """

    val expected = json"""
      {
        "errors": [
          {
            "message": $message,
            "path": ["tagCount"]
          }
        ],
        "data": {
          "ping": "pong",
          "tagCount": null
        }
      }
    """

    assertIO(FailingCount.compileAndRun(query), expected)
  }

  private val delegateQuery = """
    query {
      ping
      delegated {
        name
      }
    }
  """

  /**
   * The response for `delegateQuery`, with `data` as its data entry.
   */
  private def delegateExpected(data: Json): Json =
    json"""
      {
        "errors": [
          {
            "message": $message,
            "path": ["delegated", "name"]
          }
        ],
        "data": $data
      }
    """

  test("a failed delegated field keeps the data of the positions which succeeded") {
    val data = json"""
      {
        "ping": "pong",
        "delegated": null
      }
    """

    assertIO(NullableDelegate.compileAndRun(delegateQuery), delegateExpected(data))
  }

  test("a null from a non-null delegated field bubbles up to the data entry") {
    assertIO(NonNullDelegate.compileAndRun(delegateQuery), delegateExpected(Json.Null))
  }

  test("a response path uses the alias of the position") {
    val aliased = """
      query {
        entries: items {
          name
        }
      }
    """

    val expected = json"""["entries", 1, "name"]"""

    assertIO(
      NullableName
        .compileAndRun(aliased)
        .map(_.hcursor.downField("errors").downN(0).downField("path").focus),
      Some(expected)
    )
  }
}
