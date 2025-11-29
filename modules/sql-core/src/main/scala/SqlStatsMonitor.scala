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
package sql

import cats.Applicative
import cats.effect.Ref
import cats.syntax.all._

import SqlStatsMonitor.SqlStats

/**
 * A SqlMonitor that accumulates `SqlStats` in a `Ref`. Stage boundaries and results are not
 * tracked.
 */
abstract class SqlStatsMonitor[F[_]: Applicative, A](
  ref: Ref[F, List[SqlStats]]
) extends SqlMonitor[F, A] {

  /** Get the current state and reset it to `Nil`. */
  final def take: F[List[SqlStats]] =
    ref.getAndSet(Nil).map(_.reverse)

  final def queryMapped(query: Query, fragment: A, rows: Int, cols: Int): F[Unit] =
    ref.update { stats =>
      val (sql, args) = inspect(fragment)
      SqlStats(query, sql, args, rows, cols) :: stats
    }

  final def resultComputed(result: Result[QueryInterpreter.ProtoJson]): F[Unit] =
    Applicative[F].unit

  /** Extract the SQL string and query arguments from a fragment. */
  def inspect(fragment: A): (String, List[Any])

}

object SqlStatsMonitor {

  final case class SqlStats(
    val query: Query,
    val sql:   String,
    val args:  List[Any],
    val rows:  Int,
    val cols:  Int
  ) {

    /** Normalize whitespace in `query` for easier testing. */
    def normalize: SqlStats =
      copy(sql = sql.replaceAll("\\s+", " ").trim)

  }

}
