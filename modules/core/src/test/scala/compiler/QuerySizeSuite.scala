// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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

import cats.data.NonEmptyChain
import munit.CatsEffectSuite

import grackle.{Problem, Result}
import starwars.StarWarsMapping

class QuerySizeSuite extends CatsEffectSuite {

  test("depth 2 query") {
    val query = """
      query {
        character(id: "1000") {
          name
        }
      }
    """

    val compiledQuery = StarWarsMapping.compiler.compile(query).toOption.get.query
    val res = StarWarsMapping.querySizeValidator.querySize(compiledQuery, Map.empty)

    assertEquals(res, ((2,1)))
  }

  test("also depth 2 query") {
    val query = """
      query {
        character(id: "1000") {
          name
          id
        }
      }
    """

    val compiledQuery = StarWarsMapping.compiler.compile(query).toOption.get.query
    val res = StarWarsMapping.querySizeValidator.querySize(compiledQuery, Map.empty)

    assertEquals(res, ((2,2)))
  }

  test("depth 3 query") {
    val query = """
      query {
        character(id: "1000") {
          friends {
            name
          }
        }
      }
    """

    val compiledQuery = StarWarsMapping.compiler.compile(query).toOption.get.query
    val res = StarWarsMapping.querySizeValidator.querySize(compiledQuery, Map.empty)

    assertEquals(res, ((3,1)))
  }

  test("depth 4 query") {
    val query = """
      query {
        character(id: "1000") {
          name
          friends {
            name
            friends {
              name
            }
          }
        }
      }
    """

    val compiledQuery = StarWarsMapping.compiler.compile(query).toOption.get.query
    val res = StarWarsMapping.querySizeValidator.querySize(compiledQuery, Map.empty)

    assertEquals(res, ((4,3)))
  }
  test("aliased depth 2 query") {

    val query =
      """
      query {
        luke: character(id: "1000") {
          handle: name
        }
      }
    """

    val compiledQuery = StarWarsMapping.compiler.compile(query).toOption.get.query
    val res = StarWarsMapping.querySizeValidator.querySize(compiledQuery, Map.empty)

    assertEquals(res, ((2,1)))
  }

  test("grouplist depth 2 query") {

    val query =
      """
      query {
        character(id: "1000") {
          appearsIn
        }
      }
    """

    val compiledQuery = StarWarsMapping.compiler.compile(query).toOption.get.query
    val res = StarWarsMapping.querySizeValidator.querySize(compiledQuery, Map.empty)

    assertEquals(res, ((2,1)))
  }

  test("fragments depth 3 query") {
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

    val compiledQuery = StarWarsMapping.compiler.compile(query).toOption.get.query
    val res = StarWarsMapping.querySizeValidator.querySize(compiledQuery, Map.empty)

    assertEquals(res, ((3,5)))
  }

  test("width 2 query") {
    val query = """
      query {
        character(id: "1000") {
          name
          id
        }
      }
    """

    val compiledQuery = StarWarsMapping.compiler.compile(query).toOption.get.query
    val res = StarWarsMapping.querySizeValidator.querySize(compiledQuery, Map.empty)

    assert(res._2 == 2)
  }

  test("width 5 query") {
    val query = """
      query {
        character(id: "1000") {
          name
          friends {
            name
            id
            friends {
              name
              id
            }
          }
        }
      }
    """

    val compiledQuery = StarWarsMapping.compiler.compile(query).toOption.get.query
    val res = StarWarsMapping.querySizeValidator.querySize(compiledQuery, Map.empty)

    assert(res._2 == 5)
  }

  test("query too deep") {
    val query = """
      query {
        character(id: "1000") {
          name
          friends {
            name
            friends {
              name
              friends {
                name
                friends {
                  name
                  friends {
                    name
                    friends {
                      name
                    }
                  }
                }
              }
            }
          }
        }
      }
    """

    val expected = Problem("Query is too deep: depth is 8 levels, maximum is 5")

    val res = StarWarsMapping.compiler.compile(query)
    assertEquals(res, Result.Failure(NonEmptyChain(expected)))
  }

  test("query too wide") {
    val query = """
      query {
        character(id: "1000") {
          name
          id
          friends {
            name
            id
            friends {
              name
              id
            }
          }
        }
      }
    """


    val expected = Problem("Query is too wide: width is 6 leaves, maximum is 5")

    val res = StarWarsMapping.compiler.compile(query)
    assertEquals(res, Result.Failure(NonEmptyChain(expected)))
  }
}
