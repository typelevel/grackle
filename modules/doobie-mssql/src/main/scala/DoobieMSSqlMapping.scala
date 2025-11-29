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
import _root_.doobie.Transactor

import grackle.Mapping
import grackle.Query.OrderSelection
import grackle.doobie._
import grackle.sql._

abstract class DoobieMSSqlMapping[F[_]](
  val transactor: Transactor[F],
  val monitor:    DoobieMonitor[F],
)(
  implicit val M: Sync[F]
) extends Mapping[F] with DoobieMSSqlMappingLike[F]

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
    val casedExpr = if(caseInsensitive) Fragments.const("UPPER(") |+| expr |+| Fragments.const(s")") else expr
    val casedPattern = if(caseInsensitive) pattern.toUpperCase else pattern
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
            case "TIMESTAMP" => "DATETIMEOFFSET" // TODO: Probably shouldn't be TIMESTAMP on the LHS
            case other => other
          }
        Fragments.const(s"CAST(NULL AS $convName)")
      case _ => Fragments.const("NULL")
    }

  def collateSelected: Boolean = false

  def distinctOnToFragment(dcols: List[Fragment]): Fragment =
    Fragments.const("DISTINCT ")

  def distinctOrderColumn(owner: ColumnOwner, col: SqlColumn, predCols: List[SqlColumn], orders: List[OrderSelection[_]]): SqlColumn =
    SqlColumn.FirstValueColumn(owner, col, predCols, orders)

  def encapsulateUnionBranch(s: SqlSelect): SqlSelect =
    if(s.orders.isEmpty) s
    else s.toSubquery(s.table.name+"_encaps", Laterality.NotLateral)

  def mkLateral(inner: Boolean): Laterality =
    Laterality.Apply(inner)

  def defaultOffsetForSubquery(subquery: SqlQuery): SqlQuery =
    subquery match {
      case s: SqlSelect if s.orders.nonEmpty && s.offset.isEmpty => s.copy(offset = 0.some)
      case _ => subquery
    }

  def defaultOffsetForLimit(limit: Option[Int]): Option[Int] =
    limit.as(0)

  def orderToFragment(col: Fragment, ascending: Boolean, nullsLast: Boolean): Fragment = {
    val dir = if(ascending) Fragments.empty else Fragments.const(" DESC")
    val nulls =
      if(nullsLast && ascending)
        Fragments.const(" CASE WHEN ") |+| col |+| Fragments.const(" IS NULL THEN 1 ELSE 0 END ASC, ")
      else if(!nullsLast && !ascending)
        Fragments.const(" CASE WHEN ") |+| col |+| Fragments.const(" IS NULL THEN 0 ELSE 1 END DESC, ")
      else
        Fragments.empty

    nulls |+| col |+| dir
  }

  def nullsHigh: Boolean = false
}
