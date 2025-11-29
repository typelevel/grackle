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

package grackle
package sqlpg

import cats.MonadThrow
import cats.syntax.all._

import grackle.sql.SqlMappingLike
import Query.OrderSelection

abstract class SqlPgMapping[F[_]](implicit val M: MonadThrow[F]) extends Mapping[F] with SqlPgMappingLike[F]

trait SqlPgMappingLike[F[_]] extends SqlMappingLike[F] {
  import SqlQuery.SqlSelect
  import TableExpr.Laterality

  def collateToFragment: Fragment =
    Fragments.const(" COLLATE \"C\"")

  def aliasDefToFragment(alias: String): Fragment =
    Fragments.const(s" AS $alias")

  def offsetToFragment(offset: Fragment): Fragment =
    Fragments.const(" OFFSET ") |+| offset

  def limitToFragment(limit: Fragment): Fragment =
    Fragments.const(" LIMIT ") |+| limit

  def likeToFragment(expr: Fragment, pattern: String, caseInsensitive: Boolean): Fragment = {
    val op = if(caseInsensitive) "ILIKE" else "LIKE"
    expr |+| Fragments.const(s" $op ") |+| Fragments.bind(stringEncoder, pattern)
  }

  def ascribedNullToFragment(codec: Codec): Fragment =
    Fragments.sqlTypeName(codec) match {
      case Some(name) => Fragments.const(s"(NULL :: $name)")
      case None => Fragments.const("NULL")
    }

  def collateSelected: Boolean = true

  def distinctOnToFragment(dcols: List[Fragment]): Fragment =
    Fragments.const("DISTINCT ON ") |+| Fragments.parentheses(dcols.intercalate(Fragments.const(", ")))

  def distinctOrderColumn(owner: ColumnOwner, col: SqlColumn, predCols: List[SqlColumn], orders: List[OrderSelection[_]]): SqlColumn = col

  def encapsulateUnionBranch(s: SqlSelect): SqlSelect = s
  def mkLateral(inner: Boolean): Laterality = Laterality.Lateral
  def defaultOffsetForSubquery(subquery: SqlQuery): SqlQuery = subquery
  def defaultOffsetForLimit(limit: Option[Int]): Option[Int] = None

  def orderToFragment(col: Fragment, ascending: Boolean, nullsLast: Boolean): Fragment = {
    val dir = if(ascending) Fragments.empty else Fragments.const(" DESC")
    val nulls =
      if(!nullsLast && ascending)
        Fragments.const(" NULLS FIRST ")
      else if(nullsLast && !ascending)
        Fragments.const(" NULLS LAST ")
      else
        Fragments.empty

    col |+| dir |+| nulls
  }

  def nullsHigh: Boolean = true
}
