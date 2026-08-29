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

package grackle.doobie.sqlite

import cats.effect.Sync
import cats.syntax.all._
import org.typelevel.doobie.Transactor

import grackle.Mapping
import grackle.Query.OrderSelection
import grackle.doobie._
import grackle.sql._

abstract class DoobieSqliteMapping[F[_]](
    val transactor: Transactor[F],
    val monitor: DoobieMonitor[F]
)(
    implicit val M: Sync[F]
) extends Mapping[F]
    with DoobieSqliteMappingLike[F]

/**
 * SQLite lacks two SQL constructs the shared query builder in `grackle.sql.SqlMappingLike`
 * (`modules/sql-core`) otherwise assumes are always available; each is bridged by a dialect
 * hook that the other backends implement with their previous behavior:
 *
 *   - '''No correlated FROM-clause subqueries.''' SQLite has no `LATERAL` keyword and no other
 *     way for a subquery in the FROM clause to reference a sibling table's columns, so
 *     `mkLateral` below answers `NotLateral` - the only possible rendering - and
 *     `SqlMappingLike` derives `supportsLateralJoin = false` from that. Most queries that ask
 *     for lateral evaluation don't actually need it: the correlation is supplied independently
 *     by the `JOIN ... ON` clause `SqlSelect.nest` builds regardless of dialect. See
 *     `supportsLateralJoin`'s doc comment for the consequences (an omitted redundant predicate,
 *     gated "Case 1" fast paths) and the performance trade-off.
 *   - '''No parenthesized UNION branches.''' SQLite's compound-select grammar is
 *     `select-core (compound-operator select-core)*` - a branch can never be parenthesized,
 *     unconditionally, so `unionBranchToFragment` below renders branches bare. A branch
 *     carrying its own order, offset, or limit can't be expressed inline either and is wrapped
 *     in a derived-table subquery by `encapsulateUnionBranch`, which extends the MSSQL
 *     treatment (orders only) to offset and limit as well.
 */
trait DoobieSqliteMappingLike[F[_]] extends DoobieMappingLike[F] with SqlMappingLike[F] {
  import SqlQuery.SqlSelect
  import TableExpr.Laterality

  def collateToFragment: Fragment =
    Fragments.const(" COLLATE BINARY")

  def aliasDefToFragment(alias: String): Fragment =
    Fragments.const(s" AS $alias")

  // SQLite's LIMIT/OFFSET clause is anchored on the `LIMIT` keyword: `OFFSET` (or a comma) is
  // only legal *inside* a LIMIT clause, never as a standalone top-level clause, and never before
  // the word LIMIT. That's incompatible with the fixed `offsetToFragment |+| limitToFragment`
  // rendering order used by the shared query builder (which suits Postgres, where either order is
  // legal, and MSSQL/Oracle's OFFSET-anchored `OFFSET .. FETCH ..`). We route around this by using
  // SQLite's legacy MySQL-style comma form `LIMIT <offset>, <limit>`: offsetToFragment opens the
  // clause and limitToFragment supplies the trailing operand. The two companion hooks below
  // guarantee the pair is always complete: defaultOffsetForLimit supplies offset 0 whenever a
  // limit is present, and normalizeOffsetLimit supplies `LIMIT -1` (SQLite's documented "no upper
  // bound" idiom) whenever an explicit offset has no limit to pair with.
  def offsetToFragment(offset: Fragment): Fragment =
    Fragments.const(" LIMIT ") |+| offset |+| Fragments.const(", ")

  def limitToFragment(limit: Fragment): Fragment =
    limit

  // SQLite's LIKE is ASCII case-insensitive by default and has no ILIKE, so genuinely
  // case-sensitive matching requires the connection-level `PRAGMA case_sensitive_like = ON`
  // (there's no per-expression equivalent - callers building a Transactor for this mapping need
  // to set that pragma, e.g. via SQLiteConfig; see DoobieSqliteDatabaseSuite for a worked
  // example). Since that pragma is global to the connection, not per-query, we can't just fall
  // back to a bare LIKE for the case-insensitive branch once it's enabled - both branches need to
  // be made explicit, exactly as Oracle/MSSQL do: normalise to upper case for case-insensitive
  // matches (which is then case-insensitive regardless of the pragma), and compare as-is
  // (case-sensitive, relying on the pragma) otherwise.
  def likeToFragment(expr: Fragment, pattern: String, caseInsensitive: Boolean): Fragment = {
    val casedExpr =
      if (caseInsensitive) Fragments.const("UPPER(") |+| expr |+| Fragments.const(s")")
      else expr
    val casedPattern = if (caseInsensitive) pattern.toUpperCase else pattern
    casedExpr |+| Fragments.const(s" LIKE ") |+| Fragments.bind(stringEncoder, casedPattern)
  }

  // SQLite is dynamically typed, and its CAST accepts arbitrary type names (falling back to a
  // best-guess type affinity for anything it doesn't recognise), so a typed NULL can just reuse
  // whatever name the driver reports - no per-type remapping needed, unlike Oracle/MSSQL.
  def ascribedNullToFragment(codec: Codec): Fragment =
    Fragments.sqlTypeName(codec) match {
      case Some(name) => Fragments.const(s"CAST(NULL AS $name)")
      case None => Fragments.const("NULL")
    }

  def collateSelected: Boolean = false

  def distinctOnToFragment(dcols: List[Fragment]): Fragment =
    Fragments.const("DISTINCT ")

  def distinctOrderColumn(
      owner: ColumnOwner,
      col: SqlColumn,
      predCols: List[SqlColumn],
      orders: List[OrderSelection[_]]): SqlColumn =
    SqlColumn.FirstValueColumn(owner, col, predCols, orders)

  // A compound SELECT (UNION ALL/etc.) may only have a single ORDER BY/LIMIT/OFFSET, trailing the
  // whole compound statement - an individual branch can't carry its own, parenthesized or not.
  // Branches that do (grackle pushes a per-branch limit into paged-wrapper "items" branches, for
  // example) must be wrapped in a derived table instead, extending the MSSQL treatment of orders
  // to offset and limit as well.
  def encapsulateUnionBranch(s: SqlSelect): SqlSelect =
    if (s.orders.isEmpty && s.offset.isEmpty && s.limit.isEmpty) s
    else s.toSubquery(s.table.name + "_encaps", Laterality.NotLateral)

  // A branch of a compound select can never be parenthesized, not even a "plain" one with no
  // order/limit, so branches render bare. See unionBranchToFragment's doc comment on
  // SqlMappingLike for why dropping the parens is safe.
  def unionBranchToFragment(branch: Fragment): Fragment = branch

  // SQLite has no LATERAL/APPLY mechanism at all, so NotLateral (plain subquery, no keyword) is
  // the only possible answer; SqlMappingLike derives supportsLateralJoin = false from it, which
  // omits the parent-constraint predicate only a lateral subquery could resolve and gates the
  // "Case 1" fast paths - see that member's doc comment.
  def mkLateral(inner: Boolean): Laterality =
    Laterality.NotLateral

  // Mirror image of defaultOffsetForLimit, but at the query-tree level: a select with an
  // explicit offset but no limit gets SQLite's documented idiom for "no upper bound",
  // `LIMIT -1`, so the comma-form OFFSET/LIMIT pairing in offsetToFragment always has a second
  // operand to pair with.
  def normalizeOffsetLimit(query: SqlSelect): SqlSelect =
    if (query.offset.nonEmpty && query.limit.isEmpty) query.copy(limit = (-1).some)
    else query

  // See offsetToFragment: forcing a default offset of 0 whenever a limit is present guarantees
  // the comma-form clause is always rendered as a matched `LIMIT offset, limit` pair.
  def defaultOffsetForLimit(limit: Option[Int]): Option[Int] =
    limit.as(0)

  // Modern SQLite (>= 3.30) supports NULLS FIRST/LAST natively, but unlike Postgres/Oracle its
  // default places NULLs low (first in ASC, last in DESC), so the explicit clause is needed on
  // the mirror-image cases relative to the pg dialect - the same polarity correction the MSSQL
  // dialect makes. Pinned by NullOrderingSuite.
  def orderToFragment(col: Fragment, ascending: Boolean, nullsLast: Boolean): Fragment = {
    val dir = if (ascending) Fragments.empty else Fragments.const(" DESC")
    val nulls =
      if (nullsLast && ascending)
        Fragments.const(" NULLS LAST ")
      else if (!nullsLast && !ascending)
        Fragments.const(" NULLS FIRST ")
      else
        Fragments.empty

    col |+| dir |+| nulls
  }

  // SQLite sorts NULL as lower than any non-NULL value by default (NULLs first in ASC), the same
  // convention as MSSQL.
  def nullsHigh: Boolean = false
}
