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
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import Predicate._, Query._
import sql.{Like, SqlStatsMonitor}

import grackle.test.GraphQLResponseTests.assertWeaklyEqual

/** Tests that confirm the compiler is writing the queries we want. */
trait SqlWorldCompilerSuite extends CatsEffectSuite {

  type Fragment
  def mapping: IO[(Mapping[IO], SqlStatsMonitor[IO, Fragment])]

  /** Expected SQL string for the simple restricted query test. */
  def simpleRestrictedQuerySql: String

  /** Expected SQL string for the simple filtered query test. */
  def simpleFilteredQuerySql: String

  def filterArg: String

  test("simple restricted query") {

    val query =
      """
        query {
          country(code: "GBR") {
            name
          }
        }
      """

    val expected =
      json"""
        {
          "data": {
            "country": {
              "name": "United Kingdom"
            }
          }
        }
      """

    val prog: IO[(Json, List[SqlStatsMonitor.SqlStats], Schema)] =
      for {
        mm  <- mapping
        (map, mon) = mm
        res <- map.compileAndRun(query)
        ss  <- mon.take
      } yield (res, ss.map(_.normalize), map.schema)

    prog.map { case (res, stats, schema) =>
      assertWeaklyEqual(res, expected)

      assertEquals(stats,
        List(
          SqlStatsMonitor.SqlStats(
            Select("country", Unique(Filter(Eql(schema.ref("Country") / "code",Const("GBR")), Select("name")))),
            simpleRestrictedQuerySql,
            List("GBR"),
            1,
            2
          )
        )
      )
    }
  }

  test("simple filtered query") {

    val query =
      """
        query {
          cities(namePattern: "Linh%") {
            name
          }
        }
      """

    val expected =
      json"""
        {
          "data" : {
            "cities" : [
              {
                "name" : "Linhe"
              },
              {
                "name" : "Linhai"
              },
              {
                "name" : "Linhares"
              }
            ]
          }
        }
      """

    val prog: IO[(Json, List[SqlStatsMonitor.SqlStats], Schema)] =
      for {
        mm  <- mapping
        (map, mon) = mm
        res <- map.compileAndRun(query)
        ss  <- mon.take
      } yield (res, ss.map(_.normalize), map.schema)

    prog.map { case (res, stats, schema) =>
      assertWeaklyEqual(res, expected)

      assertEquals(stats,
        List(
          SqlStatsMonitor.SqlStats(
            Select("cities", Filter(Like(schema.ref("City") / "name","Linh%",true), Select("name"))),
            simpleFilteredQuerySql,
            List(filterArg),
            3,
            2
          )
        )
      )
    }
  }
}
