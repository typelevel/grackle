// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import io.circe.Json
import munit.CatsEffectSuite

import edu.gemini.grackle._
import syntax._

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlLikeSuite extends CatsEffectSuite {
  def mapping: QueryExecutor[IO, Json]

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
