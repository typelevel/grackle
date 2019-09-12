// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini
package grackle
package parser

import atto._, Atto._
import cats.tests.CatsSuite
import Ast._, OperationType._, OperationDefinition._, Selection._, Value._

final class ParserSuite extends CatsSuite {
  val text = """
    query {
      character(id: 1000) {
        name
      }
    }
  """

  test("simple query") {
    val expected =
      Operation(Query, None, Nil, Nil,
        List(
          Field(None, Name("character"), List((Name("id"), IntValue(1000))), Nil,
            List(
              Field(None, Name("name"), Nil, Nil, Nil)
            )
          )
        )
      )

    grackle.Parser.Document.parseOnly(text).option match {
      case Some(List(Left(q))) => assert(q == expected)
    }
  }
}
