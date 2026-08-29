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

package grackle.doobie.h2

import cats.effect.Sync
import cats.syntax.all._
import org.typelevel.doobie.Transactor

import grackle.Mapping
import grackle.Query.OrderSelection
import grackle.doobie._
import grackle.sql._

abstract class DoobieH2Mapping[F[_]](
    val transactor: Transactor[F],
    val monitor: DoobieMonitor[F]
)(
    implicit val M: Sync[F]
) extends Mapping[F]
    with DoobieH2MappingLike[F]

/**
 * H2 (REGULAR mode) is close to Postgres for the constructs the shared query builder needs -
 * ILIKE, DISTINCT ON, NULLS FIRST/LAST and parenthesized union branches are all native - with
 * three exceptions: offset/limit render as standard OFFSET .. ROWS / FETCH NEXT .. ROWS ONLY
 * (as Oracle); there is no LATERAL join, which `mkLateral` answers with `NotLateral` exactly as
 * the SQLite backend does (see `supportsLateralJoin`'s doc comment for the consequences); and
 * although modern H2 supports `NULLS FIRST`/`NULLS LAST` natively, unlike Postgres/Oracle H2's
 * default places NULLs low (first in ASC, last in DESC), so `orderToFragment` emits the
 * explicit clause on the mirror-image cases relative to the pg dialect - the same polarity
 * correction the MSSQL dialect makes.
 */
trait DoobieH2MappingLike[F[_]] extends DoobieMappingLike[F] with SqlMappingLike[F] {
  import SqlQuery.SqlSelect
  import TableExpr.Laterality

  // H2 has no per-expression COLLATE, and its default ordering is already code-point order -
  // the very thing the other dialects' COLLATE "C"/BINARY opt into - so nothing needs to be
  // emitted on the rare collated-rendering paths either.
  def collateToFragment: Fragment = Fragments.empty

  def aliasDefToFragment(alias: String): Fragment =
    Fragments.const(s" AS $alias")

  // Standard SQL OFFSET/FETCH, as Oracle renders it. In H2 each clause is independently legal,
  // in the offset-then-limit order the shared builder renders, with or without ORDER BY - so no
  // offset/limit normalization is needed (normalizeOffsetLimit below is the identity).
  def offsetToFragment(offset: Fragment): Fragment =
    Fragments.const(" OFFSET ") |+| offset |+| Fragments.const(" ROWS")

  def limitToFragment(limit: Fragment): Fragment =
    Fragments.const(" FETCH NEXT ") |+| limit |+| Fragments.const(" ROWS ONLY")

  // H2 supports ILIKE natively in REGULAR mode, same as Postgres.
  def likeToFragment(expr: Fragment, pattern: String, caseInsensitive: Boolean): Fragment = {
    val op = if (caseInsensitive) "ILIKE" else "LIKE"
    expr |+| Fragments.const(s" $op ") |+| Fragments.bind(stringEncoder, pattern)
  }

  // H2's CAST accepts any type name its driver reports.
  def ascribedNullToFragment(codec: Codec): Fragment =
    Fragments.sqlTypeName(codec) match {
      case Some(name) => Fragments.const(s"CAST(NULL AS $name)")
      case None => Fragments.const("NULL")
    }

  def collateSelected: Boolean = false

  // H2 supports DISTINCT ON with Postgres semantics (first row per group under ORDER BY).
  def distinctOnToFragment(dcols: List[Fragment]): Fragment =
    Fragments.const("DISTINCT ON ") |+| Fragments.parentheses(
      dcols.intercalate(Fragments.const(", ")))

  def distinctOrderColumn(
      owner: ColumnOwner,
      col: SqlColumn,
      predCols: List[SqlColumn],
      orders: List[OrderSelection[_]]): SqlColumn = col

  // A parenthesized compound-select branch may carry its own ORDER BY/OFFSET/FETCH inline (an
  // unparenthesized one may not - the parentheses supplied by unionBranchToFragment are
  // load-bearing), so no derived-table wrapping is needed.
  def encapsulateUnionBranch(s: SqlSelect): SqlSelect = s
  def unionBranchToFragment(branch: Fragment): Fragment = Fragments.parentheses(branch)

  // H2 has no LATERAL (or APPLY) mechanism, so NotLateral (plain subquery, no keyword) is the
  // only possible rendering; SqlMappingLike derives supportsLateralJoin = false from it - see
  // that member's doc comment.
  def mkLateral(inner: Boolean): Laterality = Laterality.NotLateral

  def normalizeOffsetLimit(query: SqlSelect): SqlSelect = query
  def defaultOffsetForLimit(limit: Option[Int]): Option[Int] = None

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

  // H2 sorts NULL below any non-NULL value by default (NULLs first in ASC), the same convention
  // as MSSQL and SQLite.
  def nullsHigh: Boolean = false
}
