// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.test

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import grackle.test.GraphQLResponseTests.assertWeaklyEqual
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

trait SqlSiblingListsSpec extends AnyFunSuite {
  def mapping: QueryExecutor[IO, Json]

  test("base query") {
    val query = """
      query {
        a(id: "id_a_1") {
          bs {
            cs { nameC }
            ds { nameD }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "a" : {
            "bs" : [
              {
                "cs" : [
                  {
                    "nameC" : "name_c"
                  }
                ],
                "ds" : [
                  {
                    "nameD" : "name_d"
                  }
                ]
              }
            ]
          }
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }
}
