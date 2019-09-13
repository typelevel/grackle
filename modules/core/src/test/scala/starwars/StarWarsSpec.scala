// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini
package grackle
package starwars

import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import Query._, Binding._

final class StarsWarsSpec extends CatsSuite {
  test("simple query") {
    /*
    val query = """
      query {
        character(id: 1000) {
          name
        }
      }
    """
    */

    val compiledQuery =
      Select("character", List(StringBinding("id", "1000"))) /
        Select("name", Nil)

    val expected = json"""
      {
        "data" : {
          "character" : {
            "name" : "Luke Skywalker"
          }
        }
      }
    """

    val res = StarWarsQueryInterpreter.run(compiledQuery)
    assert(res == expected)
  }

  test("simple nested query") {
    /*
    val query = """
      query {
        character(id: 1000) {
          name
          friends {
            name
          }
        }
      }
    """
    */

    val compiledQuery =
      Select("character", List(StringBinding("id", "1000"))) / (
        Select("name", Nil) ~
        (Select("friends", Nil) /
          Select("name", Nil))
      )

    val expected = json"""
      {
        "data" : {
          "character" : {
            "name" : "Luke Skywalker",
            "friends" : [
              {
                "name" : "Han Solo"
              },
              {
                "name" : "Leia Organa"
              },
              {
                "name" : "C-3PO"
              },
              {
                "name" : "R2-D2"
              }
            ]
          }
        }
      }
    """

    val res = StarWarsQueryInterpreter.run(compiledQuery)
    assert(res == expected)
  }
}
