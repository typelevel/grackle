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

package grackle.doobie.mariadb

import cats.effect.Sync
import cats.syntax.all._
import org.typelevel.doobie.Transactor

import grackle.Mapping
import grackle.Query.OrderSelection
import grackle.doobie._
import grackle.sql._

abstract class DoobieMariaDbMapping[F[_]](
    val transactor: Transactor[F],
    val monitor: DoobieMonitor[F]
)(
    implicit val M: Sync[F]
) extends Mapping[F]
    with DoobieMariaDbMappingLike[F]

/**
 * MariaDB dialect. Unlike MySQL, MariaDB has no LATERAL derived tables in any released version,
 * so mkLateral answers NotLateral (SqlMappingLike derives supportsLateralJoin = false from
 * that, same mechanism SQLite/H2 use - see mkLateral's comment below for the consequences).
 * Everything else - no NULLS FIRST/LAST (CASE-WHEN emulation, nulls-low polarity), no DISTINCT
 * ON (FirstValueColumn), no ILIKE (LIKE is already case-insensitive under the default *_ci
 * collations), and the LIMIT-anchored comma-form offset/limit grammar
 *   - is inherited unchanged from the MySQL dialect this was forked from.
 */
trait DoobieMariaDbMappingLike[F[_]] extends DoobieMappingLike[F] with SqlMappingLike[F] {
  import SqlQuery.SqlSelect
  import TableExpr.Laterality

  // MariaDB has no charset-agnostic binary collation: the collation named here must match the
  // character set of the columns it is applied to. utf8mb4 is MariaDB's default, so that is
  // the default here; mappings over a legacy charset (latin1, utf8mb3) must override this,
  // otherwise ordering a text column raises "COLLATION 'utf8mb4_bin' is not valid for
  // CHARACTER SET ..." at query time.
  def binaryCollation: String = "utf8mb4_bin"

  // The default utf8mb4 collations are case/accent-insensitive; the collated rendering paths
  // want code-point order, which the binary collation supplies.
  def collateToFragment: Fragment = Fragments.const(s" COLLATE $binaryCollation")

  def aliasDefToFragment(alias: String): Fragment =
    Fragments.const(s" AS $alias")

  // MariaDB's OFFSET is only legal inside a LIMIT clause, exactly like SQLite - and MariaDB
  // supports the same comma form `LIMIT <offset>, <limit>`, so the same rendering trick
  // applies: offsetToFragment opens the clause, limitToFragment supplies the second operand,
  // and the two hooks below guarantee the pair is always complete.
  def offsetToFragment(offset: Fragment): Fragment =
    Fragments.const(" LIMIT ") |+| offset |+| Fragments.const(", ")

  def limitToFragment(limit: Fragment): Fragment =
    limit

  // MariaDB has no `LIMIT -1`; its documented "no upper bound" idiom is LIMIT
  // 18446744073709551615, which doesn't fit the Option[Int] the query tree carries.
  // Int.MaxValue rows is unbounded for any practical purpose.
  def normalizeOffsetLimit(query: SqlQuery): SqlQuery =
    query match {
      case s: SqlSelect if s.offset.nonEmpty && s.limit.isEmpty =>
        s.copy(limit = Int.MaxValue.some)
      case _ => query
    }

  def defaultOffsetForLimit(limit: Option[Int]): Option[Int] =
    limit.as(0)

  // Inverted from Postgres: under MariaDB's default *_ci collations a plain LIKE is already
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

  // MariaDB's CAST vocabulary is its own restricted set, not column type names.
  def ascribedNullToFragment(codec: Codec): Fragment =
    Fragments.sqlTypeName(codec) match {
      case Some(name) =>
        val castName = name.toUpperCase match {
          case "TINYINT" | "SMALLINT" | "INTEGER" | "INT" | "BIGINT" | "BOOLEAN" | "BIT" =>
            Some("SIGNED")
          case "CHAR" | "VARCHAR" | "NVARCHAR" | "TEXT" | "CLOB" => Some("CHAR")
          case "DECIMAL" | "NUMERIC" => Some("DECIMAL")
          // CAST(NULL AS DOUBLE)/CAST(NULL AS FLOAT) both work fine on MariaDB - but an
          // ascribed NULL is only ever an inference hint, not semantically required, so a bare
          // NULL is the simplest safe rendering here; keeping it a non-cast is a style choice,
          // not a compatibility requirement (unlike the JSON case just below).
          case "FLOAT" | "REAL" | "DOUBLE" | "DOUBLE PRECISION" => None
          case "DATE" => Some("DATE")
          case "TIME" => Some("TIME")
          case "TIMESTAMP" | "DATETIME" => Some("DATETIME")
          // DoobieMapping.sqlTypeName reports a json codec's vendor type name as "JSONB".
          // MariaDB's JSON is a LONGTEXT alias, not a native CAST target -
          // CAST(NULL AS JSON) fails with "ERROR 1064 (42000): You have an error in your SQL
          // syntax". A bare NULL is the safe rendering; as above, the ascription is only ever
          // an inference hint, never semantically required.
          case "JSON" | "JSONB" => None
          case _ => None
        }
        castName match {
          case Some(cn) => Fragments.const(s"CAST(NULL AS $cn)")
          case None => Fragments.const("NULL")
        }
      case None => Fragments.const("NULL")
    }

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

  // Parenthesized union branches are legal in MariaDB and may carry inline ORDER BY/LIMIT/
  // OFFSET. A branch-level ORDER BY without LIMIT may be discarded by the optimizer, which is
  // harmless: branch orders only determine row *selection* when paired with limit/offset (and
  // normalizeOffsetLimit guarantees any offset gets a limit); pure ordering is redone by the
  // outer query or in memory. So no derived-table encapsulation is needed.
  def encapsulateUnionBranch(s: SqlSelect): SqlSelect = s
  def unionBranchToFragment(branch: Fragment): Fragment = Fragments.parentheses(branch)

  // MariaDB has no LATERAL/APPLY mechanism at all, so NotLateral (plain subquery, no keyword)
  // is the only possible answer; SqlMappingLike derives supportsLateralJoin = false from it,
  // which omits the parent-constraint predicate only a lateral subquery could resolve and
  // gates the "Case 1" fast paths - see that member's doc comment. This is the one deliberate
  // structural difference from the MySQL dialect this file was forked from (MySQL 8.0.14+ has
  // native LATERAL).
  def mkLateral(inner: Boolean): Laterality =
    Laterality.NotLateral

  // MariaDB has no NULLS FIRST/LAST. Its default is nulls-low (NULLs first in ASC, last in
  // DESC), so the two cases needing correction are the mirror image of Postgres's: a
  // sort-key prefix pushes NULLs to the requested end. Note the key polarity: NULL -> 1
  // in BOTH branches (1 sorts last under ASC = NULLS LAST; 1 sorts first under DESC =
  // NULLS FIRST). MSSQL's dialect inverts the key in its DESC branch - a known latent bug
  // there, deliberately not replicated. Pinned by NullOrderingSuite.
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

  // MariaDB sorts NULL below any non-NULL value (NULLs first in ASC), same as MSSQL/SQLite/H2.
  def nullsHigh: Boolean = false
}
