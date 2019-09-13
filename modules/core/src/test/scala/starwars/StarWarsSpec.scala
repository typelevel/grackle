// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini
package grackle
package starwars

import cats.tests.CatsSuite
import Query._, Binding._

final class StarsWarsSpec extends CatsSuite {
  val text0 = """
    query {
      character(id: 1000) {
        name
      }
    }
  """

  val query0 =
    Select("character", List(StringBinding("id", "1000"))) /
      Select("name", Nil)

  test("query0") {
    val res = StarWarsQueryInterpreter.run(query0)
    assert(res == Some("Luke Skywalker"))
  }

  val text1 = """
    query {
      character(id: 1000) {
        name
        friends {
          name
        }
      }
    }
  """

  val query1 =
    Select("character", List(StringBinding("id", "1000"))) / (
      Select("name", Nil) ~
      (Select("friends", Nil) /
        Select("name", Nil))
    )

  test("query1") {
    val res = StarWarsQueryInterpreter.run(query1)
    assert(res == List(Some("Luke Skywalker"), List(Some("Han Solo"), Some("Leia Organa"), Some("C-3PO"), Some("R2-D2"))))
  }
}
