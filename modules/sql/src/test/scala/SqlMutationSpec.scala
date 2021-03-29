// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import org.scalatest.funsuite.AnyFunSuite
import edu.gemini.grackle.QueryExecutor
import cats.effect.IO
import io.circe.Json
import io.circe.literal._

trait SqlMutationSpec extends AnyFunSuite {

  def mapping: QueryExecutor[IO, Json]

  def check(query: String, expected: Json) =
    assert(mapping.compileAndRun(query).unsafeRunSync() == expected)

  test("simple update") {
    check("""
        mutation {
          updatePopulation(id: 2, population: 12345) {
            name
            population
            country {
              name
            }
          }
        }
      """,
      json"""
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
    )
  }


}