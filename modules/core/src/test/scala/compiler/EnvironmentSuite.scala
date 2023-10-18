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

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import Query._
import Value._
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

  override val selectElaborator = SelectElaborator {
    case (QueryType, "nestedSum", List(Binding("x", IntValue(x)), Binding("y", IntValue(y)))) =>
      Elab.env("x" -> x, "y" -> y)
    case (NestedType, "sum", List(Binding("x", IntValue(x)), Binding("y", IntValue(y)))) =>
      Elab.env("x" -> x, "y" -> y)
    case (NestedSumType, "nestedSum", List(Binding("x", IntValue(x)), Binding("y", IntValue(y)))) =>
      Elab.env("x" -> x, "y" -> y)
  }
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
