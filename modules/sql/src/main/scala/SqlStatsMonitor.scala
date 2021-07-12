// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
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

  final def queryMapped(query: Query, fragment: A, table: List[Row]): F[Unit] =
    ref.update { stats =>
      val (sql, args) = inspect(fragment)
      SqlStats(query, sql, args, table.size, table.headOption.foldMap(_.elems.length)) :: stats
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
