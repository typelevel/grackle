// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import cats.effect.unsafe.implicits.global

import edu.gemini.grackle._
// import syntax._

// import grackle.test.GraphQLResponseTests.assertWeaklyEqual

trait SqlRobSpec extends AnyFunSuite {
  def mapping: QueryExecutor[IO, Json]

  test("paging") {
    val query = """
      query {
        program(programId: "$foo") {
          id
          observations(limit: 3) {
            matches {
              id
            }
          }
        }
      }
    """

    // val expected = json"""
    //   {
    //     "data" : {
    //       "countries" : {
    //         "hasMore" : true,
    //         "items" : [
    //           {
    //             "code" : "ABW",
    //             "name" : "Aruba"
    //           },
    //           {
    //             "code" : "AFG",
    //             "name" : "Afghanistan"
    //           },
    //           {
    //             "code" : "AGO",
    //             "name" : "Angola"
    //           }
    //         ]
    //       }
    //     }
    //   }
    // """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    println(res)

    true

    // assertWeaklyEqual(res, expected)
  }

}
