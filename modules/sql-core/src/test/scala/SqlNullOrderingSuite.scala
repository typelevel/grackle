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

// Pins explicit NULL placement in ORDER BY across all four (ascending x nullsLast) cases, for
// every backend.
//
// Two things shape this suite, both easy to trip over:
// - No other shared sql-core fixture orders a nullable column whose data actually contains
//   NULLs, so those suites cannot observe where a dialect puts them; hence a dedicated fixture
//   (testdata/<backend>/null-ordering.sql) with NULLs in the ordered column.
// - stripCompiled in SqlMappingLike strips Offset/Limit (SQL is authoritative for them) but
//   deliberately preserves OrderBy, so grackle core re-sorts the fetched rows in memory with
//   engine-independent NULL semantics. A dialect that renders NULL placement wrongly therefore
//   still produces correct results for any query that fetches all rows - the SQL-side placement
//   is only observable when it decides which rows survive a LIMIT cut. Every test here uses
//   limit for exactly that reason, and asserts on the surviving ids, in order.
//
// The expected surviving ids depend only on the requested `nullsLast` flag, never on the
// backend's native NULL-sort default: a correct dialect must honor an explicit request either
// way, whether that means emitting nothing (the request matches the native default) or emitting
// an explicit NULLS FIRST/LAST (or an emulation of it). That is what makes this suite portable
// across nulls-low backends (SQLite, H2, MySQL, MariaDB, SQL Server) and nulls-high backends
// (Postgres, Oracle) unchanged.

import cats.effect.IO
import munit.CatsEffectSuite

import grackle.Mapping

trait SqlNullOrderingSuite extends CatsEffectSuite {
  def mapping: Mapping[IO]

  // Fixture rows (id, v): (1, 10), (2, NULL), (3, 5), (4, NULL).
  def surviving(ascending: Boolean, nullsLast: Boolean): IO[List[Int]] = {
    val query =
      s"""
        query {
          items(ascending: $ascending, nullsLast: $nullsLast, limit: 2) {
            id
            v
          }
        }
      """
    mapping.compileAndRun(query).map { json =>
      json
        .hcursor
        .downField("data")
        .downField("items")
        .focus
        .flatMap(_.asArray)
        .getOrElse(fail(s"unexpected response: $json"))
        .toList
        .map(_.hcursor.get[Int]("id").getOrElse(fail(s"unexpected response: $json")))
    }
  }

  test("ascending, nulls last") {
    assertIO(surviving(ascending = true, nullsLast = true), List(3, 1))
  }

  test("descending, nulls first") {
    assertIO(surviving(ascending = false, nullsLast = false), List(2, 4))
  }

  test("ascending, nulls first") {
    assertIO(surviving(ascending = true, nullsLast = false), List(2, 4))
  }

  test("descending, nulls last") {
    assertIO(surviving(ascending = false, nullsLast = true), List(1, 3))
  }
}
