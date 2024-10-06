// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
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

package grackle.doobie.oracle

import cats.effect.Sync
import cats.syntax.all._
import _root_.doobie.Transactor

import grackle.Mapping
import grackle.doobie._
import grackle.sql._

abstract class DoobieOracleMapping[F[_]](
  val transactor: Transactor[F],
  val monitor:    DoobieMonitor[F],
)(
  implicit val M: Sync[F]
) extends Mapping[F] with DoobieOracleMappingLike[F]

trait DoobieOracleMappingLike[F[_]] extends DoobieMappingLike[F] with SqlMappingLike[F] {
  def collateToFragment: Fragment =
    Fragments.const(" COLLATE \"binary\"")

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
            case "INTEGER" => "NUMBER"
            case "BIGINT" => "NUMBER"
            case other => other
          }
        Fragments.const(s"TO_$convName(NULL)")
      case _ => Fragments.const("NULL")
    }

  def collateSelected: Boolean = false

  def distinctOnToFragment(dcols: List[Fragment]): Fragment =
    Fragments.const("DISTINCT ")

  def distinctOrderColumn(owner: ColumnOwner, col: SqlColumn, predCols: List[SqlColumn]): SqlColumn =
    SqlColumn.FirstValueColumn(owner, col, predCols, Nil)
}
