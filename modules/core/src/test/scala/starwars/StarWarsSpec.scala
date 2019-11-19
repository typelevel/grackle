// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package starwars

import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import edu.gemini.grackle._
import Query._, Binding._, Predicate._

import StarWarsData._
import StarWarsSchema._

final class StarWarsSpec extends CatsSuite {

  val elaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("hero", _, child) =>
        Wrap("hero", Unique(FieldEquals("id", R2D2.id), child)).rightIor
      case Select(f@("character" | "human"), List(StringBinding("id", id)), child) =>
        Wrap(f, Unique(FieldEquals("id", id), child)).rightIor
    }
  ))

  test("simple query") {
    val query = """
      query {
        character(id: "1000") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "character" : {
            "name" : "Luke Skywalker"
          }
        }
      }
    """

    val compiledQuery = Compiler.compileText(query)
    val elaboratedQuery = compiledQuery.flatMap(elaborator(_, QueryType)).right.get

    val res = StarWarsQueryInterpreter.run(elaboratedQuery)
    //println(res)

    assert(res == expected)
  }

  test("simple nested query") {
    val query = """
      query {
        character(id: "1000") {
          name
          friends {
            name
          }
        }
      }
    """

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

    val compiledQuery = Compiler.compileText(query)
    val elaboratedQuery = compiledQuery.flatMap(elaborator(_, QueryType)).right.get

    val res = StarWarsQueryInterpreter.run(elaboratedQuery)
    //println(res)

    assert(res == expected)
  }
}
