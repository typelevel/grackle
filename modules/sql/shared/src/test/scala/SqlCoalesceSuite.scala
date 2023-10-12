// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.sql.test

import cats.effect.IO
import cats.syntax.all._
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import sql.SqlStatsMonitor

import grackle.test.GraphQLResponseTests.assertWeaklyEqual

trait SqlCoalesceSuite extends CatsEffectSuite {

  type Fragment
  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO, Fragment])]

  test("simple coalesced query") {
    val query = """
      query {
        r {
          id
          ca {
            id
            a
          }
          cb {
            id
            b
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "r" : [
            {
              "id" : "R1",
              "ca" : [
                {
                  "id" : "CA1a",
                  "a" : 10
                },
                {
                  "id" : "CA1b",
                  "a" : 11
                }
              ],
              "cb" : [
              ]
            },
            {
              "id" : "R2",
              "ca" : [
                {
                  "id" : "CA2",
                  "a" : 20
                }
              ],
              "cb" : [
                {
                  "id" : "CB2a",
                  "b" : true
                },
                {
                  "id" : "CB2b",
                  "b" : false
                }
              ]
            },
            {
              "id" : "R3",
              "ca" : [
                {
                  "id" : "CA3",
                  "a" : 30
                }
              ],
              "cb" : [
                {
                  "id" : "CB3",
                  "b" : true
                }
              ]
            }
          ]
        }
      }
    """

    val prog: IO[(Json, List[SqlStatsMonitor.SqlStats])] =
      for {
        mm  <- mapping
        (map, mon) = mm
        res <- map.compileAndRun(query)
        ss  <- mon.take
      } yield (res, ss)

    prog.map { case (res, stats) =>
      assertWeaklyEqual(res, expected)

      val numQueries = stats.length
      val numCells   = stats.foldMap(s => s.rows * s.cols)

      assert(numQueries == 1)
      assert(numCells   == 40)
    }
  }
}
