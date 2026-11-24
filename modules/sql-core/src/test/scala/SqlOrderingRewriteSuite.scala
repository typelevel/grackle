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

// Pins `orderingToFragment`: a column that the database would order one way and its Scala
// `Order` another is ordered and compared in SQL through an expression that agrees with the
// `Order`.
//
// Grackle re-sorts the rows it fetches in memory, so a query that fetches every row comes out
// right whatever the SQL did. The SQL-side order is only observable where it decides which
// rows are fetched at all, which is why every test here limits or filters by the ordered
// column. Without the rewrite each of them fails: "10", "100" and "1000" are the first three
// codes as text.

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import grackle.Mapping
import grackle.test.GraphQLResponseTests.assertWeaklyEqualIO

trait SqlOrderingRewriteSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  // Fixture codes: 8, 9, 10, 11, 100, 1000 in group 1 and 7, 70, 700 in group 2. Each has a
  // threshold, another number held as text.
  def codes(args: String): IO[List[String]] =
    mapping.compileAndRun(s"query { items$args { code } }").map { json =>
      json
        .hcursor
        .downField("data")
        .downField("items")
        .focus
        .flatMap(_.asArray)
        .getOrElse(fail(s"unexpected response: $json"))
        .toList
        .map(_.hcursor.get[String]("code").getOrElse(fail(s"unexpected response: $json")))
    }

  test("a limit keeps the lowest by number") {
    assertIO(codes("(limit: 3)"), List("7", "8", "9"))
  }

  test("a limit keeps the highest by number when descending") {
    assertIO(codes("(limit: 2, descending: true)"), List("1000", "700"))
  }

  test("an inequality compares by number") {
    assertIO(codes("""(after: "9", limit: 3)"""), List("10", "11", "70"))
  }

  test("an inequality against a value with more digits") {
    assertIO(codes("""(after: "100")"""), List("700", "1000"))
  }

  // The rewrite of `code` applies to the other operand too, a column that has none of its own.
  // As text the items above their threshold would be 7, 8 and 11.
  test("an inequality between two columns compares by number") {
    assertIO(codes("(aboveThreshold: true)"), List("7", "10", "70", "100"))
  }

  // What a client paging by cursor does: the last code of a page is where the next one starts.
  test("paging by cursor yields every row once") {
    def go(after: Option[String], acc: List[String]): IO[List[String]] =
      codes(after.fold("(limit: 2)")(a => s"""(after: "$a", limit: 2)""")).flatMap { page =>
        if (page.size < 2 || acc.size > 20) IO.pure(acc ++ page)
        else go(page.lastOption, acc ++ page)
      }

    assertIO(go(None, Nil), List("7", "8", "9", "10", "11", "70", "100", "700", "1000"))
  }

  test("a nested list is limited by number") {
    val query = """
      query {
        groups {
          id
          items(limit: 2) {
            code
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "groups" : [
            {
              "id" : 1,
              "items" : [
                { "code" : "8" },
                { "code" : "9" }
              ]
            },
            {
              "id" : 2,
              "items" : [
                { "code" : "7" },
                { "code" : "70" }
              ]
            }
          ]
        }
      }
    """

    val res = mapping.compileAndRun(query)

    assertWeaklyEqualIO(res, expected)
  }
}
