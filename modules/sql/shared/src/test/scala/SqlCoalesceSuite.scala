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

  test("zoned-date-time coalesced query") {
    val query = """
      query {
        r {
          id
          ca {
            id
            a
          }
          cc {
            id
            c
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
              "cc" : [
                {
                  "id" : "CC1",
                  "c" : "2020-05-27T19:00:00Z"
                }
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
              "cc" : [
                {
                  "id" : "CC2",
                  "c" : "2004-10-19T08:23:54Z"
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
              "cc" : [
                {
                  "id" : "CC3",
                  "c" : "2014-10-19T08:23:54Z"
                }
              ]
            }
          ]
        }
      }
    """

    for {
      map  <- mapping.map(_._1)
      res <- map.compileAndRun(query)
    } yield assertWeaklyEqual(res, expected)
  }
}
