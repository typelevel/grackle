// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import Cursor.Env
import Query._, Value._
import QueryCompiler._

object EnvironmentMapping extends ValueMapping[IO] {
  val schema =
    schema"""
      type Query {
        nested: Nested!
        nestedSum(x: Int!, y: Int!): NestedSum!
      }
      type Nested {
        sum(x: Int!, y: Int!): Int!
        url: String!
        nested: Nested!
      }
      type NestedSum {
        sum: Int!
        nestedSum(x: Int!, y: Int!): NestedSum!
      }
    """

  val QueryType = schema.ref("Query")
  val NestedType = schema.ref("Nested")
  val NestedSumType = schema.ref("NestedSum")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            ValueField[Unit]("nested", _ => ()),
            ValueField[Unit]("nestedSum", _ => ())
          )
      ),
      ObjectMapping(
        tpe = NestedType,
        fieldMappings =
          List(
            CursorField("sum", sum),
            CursorField("url", url),
            ValueField[Unit]("nested", _ => ())
          )
      ),
      ObjectMapping(
        tpe = NestedSumType,
        fieldMappings =
          List(
            CursorField("sum", sum),
            ValueField[Unit]("nestedSum", _ => ())
          )
      )
    )

  def sum(c: Cursor): Result[Int] =
    (for {
      x <- c.env[Int]("x")
      y <- c.env[Int]("y")
    } yield x+y).toResult(s"Missing argument")

  def url(c: Cursor): Result[String] = {
    c.env[Boolean]("secure").map(secure =>
      if(secure) "https://localhost/" else "http://localhost/"
    ).toResult(s"Missing argument")
  }

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("nestedSum", List(Binding("x", IntValue(x)), Binding("y", IntValue(y))), child) =>
        Environment(Env("x" -> x, "y" -> y), Select("nestedSum", Nil, child)).success
    },
    NestedType -> {
      case Select("sum", List(Binding("x", IntValue(x)), Binding("y", IntValue(y))), child) =>
        Environment(Env("x" -> x, "y" -> y), Select("sum", Nil, child)).success
    },
    NestedSumType -> {
      case Select("nestedSum", List(Binding("x", IntValue(x)), Binding("y", IntValue(y))), child) =>
        Environment(Env("x" -> x, "y" -> y), Select("nestedSum", Nil, child)).success
    }
  ))
}

final class EnvironmentSuite extends CatsEffectSuite {
  test("field computed from arguments (1)") {
    val query = """
      query {
        nested {
          sum(x: 13, y: 23)
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "nested" : {
            "sum" : 36
          }
        }
      }
    """

    val res = EnvironmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("field computed from arguments (2)") {
    val query = """
      query {
        nested {
          a: sum(x: 13, y: 23)
          b: sum(x: 5, y: 7)
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "nested" : {
            "a" : 36,
            "b" : 12
          }
        }
      }
    """

    val res = EnvironmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("field computed from arguments (3)") {
    val query = """
      query {
        nested {
          sum(x: 13, y: 23)
          nested {
            sum(x: 5, y: 7)
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "nested" : {
            "sum" : 36,
            "nested" : {
              "sum" : 12
            }
          }
        }
      }
    """

    val res = EnvironmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("field computed from arguments (4)") {
    val query = """
      query {
        nestedSum(x: 13, y: 23) {
          sum
          nestedSum(x: 5, y: 7) {
            sum
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "nestedSum" : {
            "sum" : 36,
            "nestedSum" : {
              "sum" : 12
            }
          }
        }
      }
    """

    val res = EnvironmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("field computed using external value (1)") {
    val query = """
      query {
        nested {
          url
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Missing argument"
          }
        ],
        "data" : null
      }
    """

    val res = EnvironmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("field computed using external value (2)") {
    val query = """
      query {
        nested {
          url
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "nested" : {
            "url" : "https://localhost/"
          }
        }
      }
    """

    val res = EnvironmentMapping.compileAndRun(query, env = Env("secure" -> true))

    assertIO(res, expected)
  }

  test("field computed using external value (3)") {
    val query = """
      query {
        nested {
          url
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "nested" : {
            "url" : "http://localhost/"
          }
        }
      }
    """

    val res = EnvironmentMapping.compileAndRun(query, env = Env("secure" -> false))

    assertIO(res, expected)
  }

  test("field computed using external value (4)") {
    val query = """
      query {
        nested {
          nested {
            url
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "nested" : {
            "nested" : {
              "url" : "https://localhost/"
            }
          }
        }
      }
    """

    val res = EnvironmentMapping.compileAndRun(query, env = Env("secure" -> true))

    assertIO(res, expected)
  }
}
