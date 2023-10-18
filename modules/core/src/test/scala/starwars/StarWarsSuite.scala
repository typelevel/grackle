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

package starwars

import cats.implicits._
import io.circe.literal._
import munit.CatsEffectSuite

final class StarWarsSuite extends CatsEffectSuite {

  test("validate mapping") {
    val es = StarWarsMapping.validator.validateMapping()
    es match {
      case Nil => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }
  }

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

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("subtype query") {
    val query = """
      query {
        human(id: "1003") {
          name
          homePlanet
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "human" : {
            "name" : "Leia Organa",
            "homePlanet" : "Alderaan"
          }
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
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

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
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
      }
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

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
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

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("field alias") {
    val query = """
      query {
        luke: character(id: "1000") {
          handle: name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "luke" : {
            "handle" : "Luke Skywalker"
          }
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("multiple root queries") {
    val query = """
      query {
        luke: character(id: "1000") {
          name
        }
        darth: character(id: "1001") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "luke" : {
            "name" : "Luke Skywalker"
          },
          "darth" : {
            "name" : "Darth Vader"
          }
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("fragments") {
    val query = """
      query {
        character(id: "1000") {
          name
          friends {
            name
            ... on Human {
              homePlanet
              appearsIn
            }
            ... on Droid {
              primaryFunction
            }
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
                "name" : "Han Solo",
                "homePlanet" : null,
                "appearsIn" : [
                  "NEWHOPE",
                  "EMPIRE",
                  "JEDI"
                ]
              },
              {
                "name" : "Leia Organa",
                "homePlanet" : "Alderaan",
                "appearsIn" : [
                  "NEWHOPE",
                  "EMPIRE",
                  "JEDI"
                ]
              },
              {
                "name" : "C-3PO",
                "primaryFunction" : "Protocol"
              },
              {
                "name" : "R2-D2",
                "primaryFunction" : "Astromech"
              }
            ]
          }
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("count") {
    val query = """
      query {
        human(id: "1000") {
          name
          numberOfFriends
          friends {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "human" : {
            "name" : "Luke Skywalker",
            "numberOfFriends" : 4,
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

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("renamed count") {
    val query = """
      query {
        human(id: "1000") {
          name
          num:numberOfFriends
          friends {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "human" : {
            "name" : "Luke Skywalker",
            "num" : 4,
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

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("offset/limit (1)") {
    val query = """
      query {
        characters(offset: 0, limit: 5) {
          name
        }
      }
    """

    val expected = json"""
    {
      "data" : {
        "characters" : [
          {
            "name" : "Luke Skywalker"
          },
          {
            "name" : "Darth Vader"
          },
          {
            "name" : "Han Solo"
          },
          {
            "name" : "Leia Organa"
          },
          {
            "name" : "Wilhuff Tarkin"
          }
        ]
      }
    }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("offset/limit (2)") {
    val query = """
      query {
        characters(offset: 2, limit: 5) {
          name
        }
      }
    """

    val expected = json"""
    {
      "data" : {
        "characters" : [
          {
            "name" : "Han Solo"
          },
          {
            "name" : "Leia Organa"
          },
          {
            "name" : "Wilhuff Tarkin"
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
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }
}
