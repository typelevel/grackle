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

package grackle.sql.test

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlLikeSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  test("No filter") {
    val query = """
      query {
        likes {
          id
          notNullable
          nullable
        }
      }

    """

    val expected = json"""
      {
        "data" : {
          "likes" : [
            {
              "id" : 1,
              "notNullable" : "foo",
              "nullable" : null
            },
            {
              "id" : 2,
              "notNullable" : "bar",
              "nullable" : "baz"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("Not nullable, not null") {
    val query = """
      query {
        likeNotNullableNotNullable(pattern: "f%") {
          id
          notNullable
          nullable
        }
      }

    """

    val expected = json"""
      {
        "data" : {
          "likeNotNullableNotNullable" : [
            {
              "id" : 1,
              "notNullable" : "foo",
              "nullable" : null
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("Not nullable, null") {
    val query = """
      query {
        likeNotNullableNullable(pattern: null) {
          id
          notNullable
          nullable
        }
      }

    """

    val expected = json"""
      {
        "data" : {
          "likeNotNullableNullable" : [
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("Nullable, not null") {
    val query = """
      query {
        likeNullableNotNullable(pattern: "b%") {
          id
          notNullable
          nullable
        }
      }

    """

    val expected = json"""
      {
        "data" : {
          "likeNullableNotNullable" : [
            {
              "id" : 2,
              "notNullable" : "bar",
              "nullable" : "baz"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  test("Nullable, null") {
    val query = """
      query {
        likeNullableNullable(pattern: null) {
          id
          notNullable
          nullable
        }
      }

    """

    val expected = json"""
      {
        "data" : {
          "likeNullableNullable" : [
            {
              "id" : 1,
              "notNullable" : "foo",
              "nullable" : null
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
