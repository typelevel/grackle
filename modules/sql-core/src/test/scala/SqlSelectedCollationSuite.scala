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

package grackle.sql.test

import cats.effect.IO
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.sql.SqlStatsMonitor
import grackle.test.GraphQLResponseTests.assertWeaklyEqual

/**
 * Selected columns must not be collated: a collated key cannot be joined through the column's
 * index. Ordering and DISTINCT ON are still collated.
 */
trait SqlSelectedCollationSuite extends CatsEffectSuite {

  type Fragment
  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO, Fragment])]

  private val collate = "COLLATE \"C\""
  private val collated = """\(\s*[\w.]+ COLLATE "C"\s*\)"""
  private val ordered = s"""$collated( DESC)?( NULLS (FIRST|LAST))?"""
  private val distinctOn = s"""DISTINCT ON \\(\\s*$collated(\\s*,\\s*$collated)*\\s*\\)""".r
  private val orderBy = s"""ORDER BY $ordered(\\s*,\\s*$ordered)*""".r

  test("paged query ordered on a text key does not collate selected columns") {

    val query =
      """
        query {
          root(limit: 1) {
            listA {
              id
              elemA
            }
            listB {
              id
              elemB
            }
          }
        }
      """

    val expected = json"""
      {
        "data" : {
          "root" : [
            {
              "listA" : [
                { "id" : "a0", "elemA" : "foo" },
                { "id" : "a1", "elemA" : "bar" },
                { "id" : "a2", "elemA" : "baz" },
                { "id" : "a3", "elemA" : "quux" }
              ],
              "listB" : [
                { "id" : "b0", "elemB" : 23 },
                { "id" : "b1", "elemB" : 13 },
                { "id" : "b2", "elemB" : 17 },
                { "id" : "b3", "elemB" : 11 }
              ]
            }
          ]
        }
      }
    """

    val prog: IO[(Json, List[String])] =
      for {
        mm <- mapping
        (map, mon) = mm
        res <- map.compileAndRun(query)
        ss <- mon.take
      } yield (res, ss.map(_.normalize.sql))

    prog.map {
      case (res, sqls) =>
        assertWeaklyEqual(res, expected)

        val sql = sqls.mkString(" ")
        assert(
          distinctOn.findFirstIn(sql).isDefined,
          s"expected a collated DISTINCT ON in: $sql")
        assert(orderBy.findFirstIn(sql).isDefined, s"expected a collated ORDER BY in: $sql")

        val rest = orderBy.replaceAllIn(distinctOn.replaceAllIn(sql, ""), "")
        assert(!rest.contains(collate), s"collated selected column in: $sql")
    }
  }
}
