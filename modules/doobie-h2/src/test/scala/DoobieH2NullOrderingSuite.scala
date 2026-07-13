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

package grackle.doobie.h2.test

// Pins explicit NULL placement in ORDER BY across all four (ascending x nullsLast) cases;
// mirror of the doobie-sqlite NullOrderingSuite, over the same fixture shape.
//
// Two things shape this suite, both easy to trip over:
// - No shared sql-core fixture orders a nullable column whose data actually contains NULLs, so
//   the shared suites cannot observe where a dialect puts them; hence a local fixture
//   (testdata/h2/null-ordering.sql) with NULLs in the ordered column.
// - stripCompiled in SqlMappingLike strips Offset/Limit (SQL is authoritative for them) but
//   deliberately preserves OrderBy, so grackle core re-sorts the fetched rows in memory with
//   engine-independent NULL semantics. A dialect that renders NULL placement wrongly therefore
//   still produces correct results for any query that fetches all rows - the SQL-side placement
//   is only observable when it decides which rows survive a LIMIT cut. Every test here uses
//   limit for exactly that reason, and asserts on the surviving ids, in order.
//
// H2 sorts NULLs low by default (first in ASC, last in DESC), so the two cases requesting the
// opposite placement only work if the dialect emits an explicit NULLS FIRST/LAST.

import cats.effect.IO

import grackle.Query.{Binding, Limit, OrderBy, OrderSelection, OrderSelections}
import grackle.QueryCompiler.{Elab, SelectElaborator}
import grackle.Value.{BooleanValue, IntValue}
import grackle.sql.test._
import grackle.syntax._

trait NullOrderingMapping[F[_]] extends SqlTestMapping[F] {

  object nullOrdering extends TableDef("null_ordering") {
    val id = col("id", int4)
    val v = col("v", nullable(int4))
  }

  val schema =
    schema"""
      type Query {
        items(ascending: Boolean!, nullsLast: Boolean!, limit: Int!): [Item!]!
      }
      type Item {
        id: Int!
        v: Int
      }
    """

  val QueryType = schema.ref("Query")
  val ItemType = schema.ref("Item")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlObject("items")
        )
      ),
      ObjectMapping(
        tpe = ItemType,
        fieldMappings = List(
          SqlField("id", nullOrdering.id, key = true),
          SqlField("v", nullOrdering.v)
        )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (
          QueryType,
          "items",
          List(
            Binding("ascending", BooleanValue(asc)),
            Binding("nullsLast", BooleanValue(nl)),
            Binding("limit", IntValue(lim)))) =>
      Elab.transformChild(child =>
        Limit(
          lim,
          OrderBy(
            OrderSelections(
              List(
                OrderSelection[Option[Int]](ItemType / "v", ascending = asc, nullsLast = nl),
                // Secondary key so the order within the NULL block is deterministic.
                OrderSelection[Int](ItemType / "id")
              )),
            child
          )
        ).success)
  }
}

final class NullOrderingSuite extends DoobieH2DatabaseSuite {
  lazy val mapping = new DoobieH2TestMapping(transactor) with NullOrderingMapping[IO]

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

  test("ascending, nulls last (non-default for H2)") {
    assertIO(surviving(ascending = true, nullsLast = true), List(3, 1))
  }

  test("descending, nulls first (non-default for H2)") {
    assertIO(surviving(ascending = false, nullsLast = false), List(2, 4))
  }

  test("ascending, nulls first (H2's default placement)") {
    assertIO(surviving(ascending = true, nullsLast = false), List(2, 4))
  }

  test("descending, nulls last (H2's default placement)") {
    assertIO(surviving(ascending = false, nullsLast = true), List(1, 3))
  }
}
