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

package grackle.sql.test

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._

import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlMutationSuite extends CatsEffectSuite {

  def mapping: Mapping[IO]

  test("simple read") {
    val query = """
      query {
        city(id: 2) {
          name
          population
          country {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "city" : {
            "name" : "Qandahar",
            "population" : 237500,
            "country" : {
              "name" : "Afghanistan"
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  // In this test the query is fully elaborated prior to execution.
  test("simple update") {
    val query = """
      mutation {
        updatePopulation(id: 2, population: 12345) {
          name
          population
          country {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "updatePopulation" : {
            "name" : "Qandahar",
            "population" : 12345,
            "country" : {
              "name" : "Afghanistan"
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }

  // In this test the query must be elaborated *after* execution of the effect because the ID
  // of the inserted city isn't known until then.
  test("insert") {
    val query = """
      mutation {
        createCity(name: "Wiggum", countryCode: "USA", population: 789) {
          name
          population
          country {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "createCity" : {
            "name" : "Wiggum",
            "population" : 789,
            "country" : {
              "name" : "United States"
            }
          }
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
