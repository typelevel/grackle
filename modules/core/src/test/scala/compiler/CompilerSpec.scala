// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini
package grackle
package compiler

import cats.tests.CatsSuite

import Query._, Binding._

final class CompilerSuite extends CatsSuite {
  test("simple query") {
    val text = """
      query {
        character(id: "1000") {
          name
        }
      }
    """

    val expected =
      SelectObject("character", List(StringBinding("id", "1000")),
        SelectLeaf("name", Nil)
      )

    val res = Compiler.compileText(text)
    assert(res == Some(expected))
  }

  test("simple nested query") {
    val text = """
      query {
        character(id: "1000") {
          name
          friends {
            name
          }
        }
      }
    """

    val expected =
      SelectObject(
        "character", List(StringBinding("id", "1000")),
        SelectLeaf("name", Nil) ~
          SelectObject(
            "friends", Nil,
            SelectLeaf("name", Nil)
          )
      )

    val res = Compiler.compileText(text)
    assert(res == Some(expected))
  }
}
