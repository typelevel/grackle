// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.IO
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import cats.effect.unsafe.implicits.global

import edu.gemini.grackle._
import syntax._

import grackle.test.GraphQLResponseTests.assertWeaklyEqual

trait SqlInterfacesSpec2 extends AnyFunSuite {
  def mapping: QueryExecutor[IO, Json]

  test("when discriminator fails the fragments should be ignored") {
    val query = """
      query {
        entities {
          id
          entityType
          title
          ... on Film {
            rating
          }
          ... on Series {
            numberOfEpisodes
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "entities" : [
            {
              "id" : "1",
              "entityType" : "FILM",
              "title" : "Film 1"
            },
            {
              "id" : "2",
              "entityType" : "FILM",
              "title" : "Film 2"
            },
            {
              "id" : "3",
              "entityType" : "FILM",
              "title" : "Film 3"
            },
            {
              "id" : "4",
              "entityType" : "SERIES",
              "title" : "Series 1"
            },
            {
              "id" : "5",
              "entityType" : "SERIES",
              "title" : "Series 2"
            },
            {
              "id" : "6",
              "entityType" : "SERIES",
              "title" : "Series 3"
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query).unsafeRunSync()
    //println(res)

    assertWeaklyEqual(res, expected)
  }
}
