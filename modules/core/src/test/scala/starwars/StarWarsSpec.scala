// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package starwars

import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

final class StarWarsSpec extends CatsSuite {

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

    val compiledQuery = StarWarsQueryCompiler.compile(query).right.get
    val res = StarWarsQueryInterpreter.run(compiledQuery, StarWarsSchema.queryType)
    //println(res)

    assert(res == expected)
  }

  test("simple nested query (1)") {
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

    val compiledQuery = StarWarsQueryCompiler.compile(query).right.get
    val res = StarWarsQueryInterpreter.run(compiledQuery, StarWarsSchema.queryType)
    //println(res)

    assert(res == expected)
  }

  test("simple nested query (2)") {
    val query = """
      query {
        character(id: "1000") {
          name
          friends {
            name
            id
            appearsIn
          }
        }
      }}
    """

    val expected = json"""
      {
        "data" : {
          "character" : {
            "name" : "Luke Skywalker",
            "friends" : [
              {
                "name" : "Han Solo",
                "id" : "1002",
                "appearsIn" : [
                  "NEWHOPE",
                  "EMPIRE",
                  "JEDI"
                ]
              },
              {
                "name" : "Leia Organa",
                "id" : "1003",
                "appearsIn" : [
                  "NEWHOPE",
                  "EMPIRE",
                  "JEDI"
                ]
              },
              {
                "name" : "C-3PO",
                "id" : "2000",
                "appearsIn" : [
                  "NEWHOPE",
                  "EMPIRE",
                  "JEDI"
                ]
              },
              {
                "name" : "R2-D2",
                "id" : "2001",
                "appearsIn" : [
                  "NEWHOPE",
                  "EMPIRE",
                  "JEDI"
                ]
              }
            ]
          }
        }
      }
    """

    val compiledQuery = StarWarsQueryCompiler.compile(query).right.get
    val res = StarWarsQueryInterpreter.run(compiledQuery, StarWarsSchema.queryType)
    //println(res)

    assert(res == expected)
  }

  test("query with enum argument") {
    val query = """
      query {
        hero(episode: EMPIRE) {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "hero" : {
            "name" : "Luke Skywalker"
          }
        }
      }
    """

    val compiledQuery = StarWarsQueryCompiler.compile(query).right.get
    val res = StarWarsQueryInterpreter.run(compiledQuery, StarWarsSchema.queryType)
    //println(res)

    assert(res == expected)
  }
}
