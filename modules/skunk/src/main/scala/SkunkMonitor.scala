// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package skunk

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import _root_.skunk.AppliedFragment

import QueryInterpreter.ProtoJson
import sql._

case class SkunkStats(
  query: Query,
  sql: String,
  args: Any,
  rows: Int,
  cols: Int
)

object SkunkMonitor {

  def noopMonitor[F[_]: Applicative]: SkunkMonitor[F] =
    new SkunkMonitor[F] {
      def queryMapped(query: Query, fragment: AppliedFragment, table: List[Row]): F[Unit] = ().pure[F]
      def resultComputed(result: Result[ProtoJson]): F[Unit] = ().pure[F]
    }

  def statsMonitor[F[_]: Sync]: F[SqlStatsMonitor[F, AppliedFragment]] =
    Ref[F].of(List.empty[SqlStatsMonitor.SqlStats]).map { ref =>
      new SqlStatsMonitor[F, AppliedFragment](ref) {
        def inspect(af: AppliedFragment): (String, List[Any]) = {
          val (f, a) = (af.fragment, af.argument)
          (f.sql, f.encoder.encode(a).map(_.getOrElse("null")))
        }
      }
    }

}
