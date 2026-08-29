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

package grackle.doobie.mssql

import cats.effect.Sync
import cats.syntax.all._
import org.typelevel.doobie.Transactor

import grackle.Mapping
import grackle.Query.OrderSelection
import grackle.doobie._
import grackle.sql._

abstract class DoobieMSSqlMapping[F[_]](
    val transactor: Transactor[F],
    val monitor: DoobieMonitor[F]
)(
    implicit val M: Sync[F]
) extends Mapping[F]
    with DoobieMSSqlMappingLike[F]

trait DoobieMSSqlMappingLike[F[_]] extends DoobieMappingLike[F] with SqlMappingLike[F] {
  import SqlQuery.SqlSelect
  import TableExpr.Laterality

  def collateToFragment: Fragment =
    Fragments.const(" COLLATE DATABASE_DEFAULT")

  def aliasDefToFragment(alias: String): Fragment =
    Fragments.const(s" $alias")

  def offsetToFragment(offset: Fragment): Fragment =
    Fragments.const(" OFFSET ") |+| offset |+| Fragments.const(" ROWS")

  def limitToFragment(limit: Fragment): Fragment =
    Fragments.const(" FETCH FIRST ") |+| limit |+| Fragments.const(" ROWS ONLY")

  def likeToFragment(expr: Fragment, pattern: String, caseInsensitive: Boolean): Fragment = {
    val casedExpr =
      if (caseInsensitive) Fragments.const("UPPER(") |+| expr |+| Fragments.const(s")")
      else expr
    val casedPattern = if (caseInsensitive) pattern.toUpperCase else pattern
    casedExpr |+| Fragments.const(s" LIKE ") |+| Fragments.bind(stringEncoder, casedPattern)
  }

  def ascribedNullToFragment(codec: Codec): Fragment =
    Fragments.sqlTypeName(codec) match {
      case Some(name) if !name.startsWith("_") =>
        val convName =
          name match {
            case "VARCHAR" => "CHAR"
            case "NVARCHAR" => "NCHAR"
            case "INTEGER" => "INTEGER"
            case "BIGINT" => "BIGINT"
            case "BOOLEAN" => "BIT"
            case "TIMESTAMP" =>
              "DATETIMEOFFSET" // TODO: Probably shouldn't be TIMESTAMP on the LHS
            case other => other
          }
        Fragments.const(s"CAST(NULL AS $convName)")
      case _ => Fragments.const("NULL")
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

  def encapsulateUnionBranch(s: SqlSelect): SqlSelect =
    if (s.orders.isEmpty) s
    else
      // The subquery name lands in alias position, so a schema-qualified table name must be
      // folded to a bare identifier first (issue #342).
      s.toSubquery(s.table.identifier + "_encaps", Laterality.NotLateral)

  def unionBranchToFragment(branch: Fragment): Fragment = Fragments.parentheses(branch)

  def mkLateral(inner: Boolean): Laterality =
    Laterality.Apply(inner)

  // MSSQL's grammar requires an ORDER BY inside a derived table to be paired with an
  // OFFSET/FETCH clause; at the query root the pairing is optional and OFFSET 0 ROWS is a
  // harmless no-op, so the default can be supplied unconditionally.
  def normalizeOffsetLimit(query: SqlSelect): SqlSelect =
    if (query.orders.nonEmpty && query.offset.isEmpty) query.copy(offset = 0.some)
    else query

  def defaultOffsetForLimit(limit: Option[Int]): Option[Int] =
    limit.as(0)

  def orderToFragment(col: Fragment, ascending: Boolean, nullsLast: Boolean): Fragment = {
    val dir = if (ascending) Fragments.empty else Fragments.const(" DESC")
    // SQL Server has no NULLS FIRST/LAST and sorts NULLs low (first in ASC, last in DESC), so
    // the two requests that ask for the opposite placement are relocated by a leading sort key
    // which maps NULL to 1 and everything else to 0: sorted ASC that puts NULLs last, sorted
    // DESC it puts them first. Note the key is the same in both branches - only the direction
    // it is sorted in differs. Pinned by NullOrderingSuite.
    val nulls =
      if (nullsLast && ascending)
        Fragments.const(" CASE WHEN ") |+| col |+| Fragments.const(
          " IS NULL THEN 1 ELSE 0 END ASC, ")
      else if (!nullsLast && !ascending)
        Fragments.const(" CASE WHEN ") |+| col |+| Fragments.const(
          " IS NULL THEN 1 ELSE 0 END DESC, ")
      else
        Fragments.empty

    nulls |+| col |+| dir
  }

  def nullsHigh: Boolean = false
}
