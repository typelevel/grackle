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

package grackle.doobie.mysql

import cats.effect.Sync
import cats.syntax.all._
import org.typelevel.doobie.Transactor

import grackle.Mapping
import grackle.Query.OrderSelection
import grackle.doobie._
import grackle.sql._

abstract class DoobieMySqlMapping[F[_]](
    val transactor: Transactor[F],
    val monitor: DoobieMonitor[F]
)(
    implicit val M: Sync[F]
) extends Mapping[F]
    with DoobieMySqlMappingLike[F]

/**
 * MySQL 8.0.14+ dialect.
 *
 * LATERAL derived tables (8.0.14) and window functions (8.0) are native, so the two deep
 * structural hooks are pg-like; the differences are all local rendering: no NULLS FIRST/LAST
 * (CASE-WHEN emulation, nulls-low polarity), no DISTINCT ON (FirstValueColumn, as MSSQL), no
 * ILIKE (LIKE is already case-insensitive under the default *_ci collations - it's the
 * case-SENSITIVE branch that needs help), and a LIMIT-anchored offset/limit grammar identical
 * to SQLite's comma form, handled the same way.
 */
trait DoobieMySqlMappingLike[F[_]] extends DoobieMappingLike[F] with SqlMappingLike[F] {
  import SqlQuery.SqlSelect
  import TableExpr.Laterality

  // MySQL has no charset-agnostic binary collation: the collation named here must match the
  // character set of the columns it is applied to. utf8mb4 is MySQL 8's default, so that is
  // the default here; mappings over a legacy charset (latin1, utf8mb3) must override this,
  // otherwise ordering a text column raises "COLLATION 'utf8mb4_bin' is not valid for
  // CHARACTER SET ..." at query time.
  def binaryCollation: String = "utf8mb4_bin"

  // The default utf8mb4 collations are case/accent-insensitive; the collated rendering paths
  // want code-point order, which the binary collation supplies.
  def collateToFragment: Fragment = Fragments.const(s" COLLATE $binaryCollation")

  def aliasDefToFragment(alias: String): Fragment =
    Fragments.const(s" AS $alias")

  // MySQL's OFFSET is only legal inside a LIMIT clause, exactly like SQLite - and MySQL
  // supports the same comma form `LIMIT <offset>, <limit>`, so the same rendering trick
  // applies: offsetToFragment opens the clause, limitToFragment supplies the second operand,
  // and the two hooks below guarantee the pair is always complete.
  def offsetToFragment(offset: Fragment): Fragment =
    Fragments.const(" LIMIT ") |+| offset |+| Fragments.const(", ")

  def limitToFragment(limit: Fragment): Fragment =
    limit

  // MySQL has no `LIMIT -1`; its documented "no upper bound" idiom is LIMIT
  // 18446744073709551615, which doesn't fit the Option[Int] the query tree carries.
  // Int.MaxValue rows is unbounded for any practical purpose; a result set longer than that
  // would be cut short with no error rather than reported.
  def normalizeOffsetLimit(query: SqlSelect): SqlSelect =
    if (query.offset.nonEmpty && query.limit.isEmpty) query.copy(limit = Int.MaxValue.some)
    else query

  def defaultOffsetForLimit(limit: Option[Int]): Option[Int] =
    limit.as(0)

  // Inverted from Postgres: under MySQL's default *_ci collations a plain LIKE is already
  // case-insensitive, so the case-SENSITIVE branch is the one needing an explicit collation.
  // The case-insensitive branch still normalizes through UPPER (as Oracle/MSSQL/SQLite do)
  // so it works under any column collation, not just the default.
  def likeToFragment(expr: Fragment, pattern: String, caseInsensitive: Boolean): Fragment =
    if (caseInsensitive) {
      Fragments.const("UPPER(") |+| expr |+| Fragments.const(") LIKE ") |+|
        Fragments.bind(stringEncoder, pattern.toUpperCase)
    } else {
      expr |+| Fragments.const(s" COLLATE $binaryCollation LIKE ") |+|
        Fragments.bind(stringEncoder, pattern)
    }

  // MySQL's CAST vocabulary is its own restricted set, not column type names.
  def ascribedNullToFragment(codec: Codec): Fragment =
    Fragments.sqlTypeName(codec) match {
      case Some(name) =>
        val castName = name.toUpperCase match {
          case "TINYINT" | "SMALLINT" | "INTEGER" | "INT" | "BIGINT" | "BOOLEAN" | "BIT" =>
            Some("SIGNED")
          case "CHAR" | "VARCHAR" | "NVARCHAR" | "TEXT" | "CLOB" => Some("CHAR")
          case "DECIMAL" | "NUMERIC" => Some("DECIMAL")
          // CAST(... AS DOUBLE/FLOAT) only exists since MySQL 8.0.17, above this dialect's
          // 8.0.14 floor - a bare NULL is legal everywhere and float-typed ascription is
          // only an inference hint, so it is the safe rendering here.
          case "FLOAT" | "REAL" | "DOUBLE" | "DOUBLE PRECISION" => None
          case "DATE" => Some("DATE")
          case "TIME" => Some("TIME")
          case "TIMESTAMP" | "DATETIME" => Some("DATETIME")
          // DoobieMapping.sqlTypeName reports a json codec's vendor type name as "JSONB".
          case "JSON" | "JSONB" => Some("JSON")
          case _ => None
        }
        castName match {
          case Some(cn) => Fragments.const(s"CAST(NULL AS $cn)")
          case None => Fragments.const("NULL")
        }
      case None => Fragments.const("NULL")
    }

  // The collation MySQL needs is applied in the ORDER BY and LIKE fragments themselves, so a
  // selected column that is also an order column needs no COLLATE of its own, and adding one
  // would only change the collation of the value handed back. Postgres is the only backend
  // here that answers true.
  def collateSelected: Boolean = false

  // No DISTINCT ON - plain DISTINCT plus the FirstValueColumn window strategy, as MSSQL.
  def distinctOnToFragment(dcols: List[Fragment]): Fragment =
    Fragments.const("DISTINCT ")

  def distinctOrderColumn(
      owner: ColumnOwner,
      col: SqlColumn,
      predCols: List[SqlColumn],
      orders: List[OrderSelection[_]]): SqlColumn =
    SqlColumn.FirstValueColumn(owner, col, predCols, orders)

  // Parenthesized union branches are legal in MySQL and may carry inline ORDER BY/LIMIT/
  // OFFSET. A branch-level ORDER BY without LIMIT may be discarded by the optimizer, which is
  // harmless: branch orders only determine row *selection* when paired with limit/offset (and
  // normalizeOffsetLimit guarantees any offset gets a limit); pure ordering is redone by the
  // outer query or in memory. So no derived-table encapsulation is needed.
  def encapsulateUnionBranch(s: SqlSelect): SqlSelect = s
  def unionBranchToFragment(branch: Fragment): Fragment = Fragments.parentheses(branch)

  // Native LATERAL derived tables, as Oracle - this is what sets the 8.0.14 version floor.
  def mkLateral(inner: Boolean): Laterality = Laterality.Lateral

  // MySQL has no NULLS FIRST/LAST. Its default is nulls-low (NULLs first in ASC, last in
  // DESC), so the two cases needing correction are the mirror image of Postgres's: a
  // sort-key prefix pushes NULLs to the requested end. Note the key polarity: NULL -> 1
  // in BOTH branches (1 sorts last under ASC = NULLS LAST; 1 sorts first under DESC =
  // NULLS FIRST), only the direction it is sorted in differs. MSSQL renders the same shape.
  // Pinned by NullOrderingSuite.
  def orderToFragment(col: Fragment, ascending: Boolean, nullsLast: Boolean): Fragment = {
    val dir = if (ascending) Fragments.empty else Fragments.const(" DESC")
    val nulls =
      if (nullsLast && ascending)
        Fragments.const("CASE WHEN ") |+| col |+|
          Fragments.const(" IS NULL THEN 1 ELSE 0 END ASC, ")
      else if (!nullsLast && !ascending)
        Fragments.const("CASE WHEN ") |+| col |+|
          Fragments.const(" IS NULL THEN 1 ELSE 0 END DESC, ")
      else
        Fragments.empty

    nulls |+| col |+| dir
  }

  // MySQL sorts NULL below any non-NULL value (NULLs first in ASC), same as MSSQL/SQLite/H2.
  def nullsHigh: Boolean = false
}
