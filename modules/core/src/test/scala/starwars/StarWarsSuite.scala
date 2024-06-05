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

import io.circe.literal._
import munit.CatsEffectSuite

final class StarWarsSuite extends CatsEffectSuite {
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

  test("interface variant (1)") {
    val query = """
      query {
        characters(offset: 3, limit: 4) {
          ... on Human {
            taggedId1
          }
          ... on Droid {
            taggedId1
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "characters" : [
            {
              "taggedId1" : "human-1003"
            },
            {
              "taggedId1" : "human-1004"
            },
            {
              "taggedId1" : "character-2000"
            },
            {
              "taggedId1" : "character-2001"
            }
          ]
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("interface variant (2)") {
    val query = """
      query {
        characters(offset: 3, limit: 4) {
          taggedId1
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "characters" : [
            {
              "taggedId1" : "human-1003"
            },
            {
              "taggedId1" : "human-1004"
            },
            {
              "taggedId1" : "character-2000"
            },
            {
              "taggedId1" : "character-2001"
            }
          ]
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("interface variant (3)") {
    val query = """
      query {
        characters(offset: 3, limit: 4) {
          taggedId1
          ... on Human {
            taggedId1
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "characters" : [
            {
              "taggedId1" : "human-1003"
            },
            {
              "taggedId1" : "human-1004"
            },
            {
              "taggedId1" : "character-2000"
            },
            {
              "taggedId1" : "character-2001"
            }
          ]
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("interface variant (4)") {
    val query = """
      query {
        characters(offset: 3, limit: 4) {
          ... on Human {
            taggedId2
          }
          ... on Droid {
            taggedId2
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "characters" : [
            {
              "taggedId2" : "human2-1003"
            },
            {
              "taggedId2" : "human2-1004"
            },
            {
              "taggedId2" : "droid2-2000"
            },
            {
              "taggedId2" : "droid2-2001"
            }
          ]
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("interface variant (5)") {
    val query = """
      query {
        characters(offset: 3, limit: 4) {
          taggedId2
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "characters" : [
            {
              "taggedId2" : "human2-1003"
            },
            {
              "taggedId2" : "human2-1004"
            },
            {
              "taggedId2" : "droid2-2000"
            },
            {
              "taggedId2" : "droid2-2001"
            }
          ]
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("interface variant (6)") {
    val query = """
      query {
        characters(offset: 3, limit: 4) {
          taggedId2
          ... on Human {
            taggedId2
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "characters" : [
            {
              "taggedId2" : "human2-1003"
            },
            {
              "taggedId2" : "human2-1004"
            },
            {
              "taggedId2" : "droid2-2000"
            },
            {
              "taggedId2" : "droid2-2001"
            }
          ]
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }
}
