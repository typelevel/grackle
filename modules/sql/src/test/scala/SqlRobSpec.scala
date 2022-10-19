// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import edu.gemini.grackle.QueryExecutor
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

trait SqlRobSpec extends AnyFunSuite {
  def mapping: QueryExecutor[IO, Json]
  test("paging") {
    mapping.compileAndRun(
      """
        query {
          program(programId: "foo") {
            id
            observations {
              matches {
                id
              }
            }
          }
        }
      """
    ).unsafeRunSync()
  }

}
