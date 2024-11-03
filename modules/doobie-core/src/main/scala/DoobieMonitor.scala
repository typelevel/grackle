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

package grackle
package doobie

import _root_.doobie.Fragment
import cats.Applicative
import cats.implicits._
import grackle.QueryInterpreter.ProtoJson
import org.typelevel.log4cats.Logger
import grackle.sql.SqlStatsMonitor
import cats.effect.Ref
import cats.effect.Sync

case class DoobieStats(
  query: Query,
  sql: String,
  args: List[Any],
  rows: Int,
  cols: Int
)

object DoobieMonitor {

  def noopMonitor[F[_]: Applicative]: DoobieMonitor[F] =
    new DoobieMonitor[F] {
      def queryMapped(query: Query, fragment: Fragment, rows: Int, cols: Int): F[Unit] = ().pure[F]
      def resultComputed(result: Result[ProtoJson]): F[Unit] = ().pure[F]
    }

  def loggerMonitor[F[_]](logger: Logger[F]): DoobieMonitor[F] =
    new DoobieMonitor[F] {

      def queryMapped(query: Query, fragment: Fragment, rows: Int, cols: Int): F[Unit] =
        logger.info(
          s"""query: $query
             |sql: ${fragment.internals.sql}
             |args: ${fragment.internals.elements.mkString(", ")}
             |fetched $rows row(s) of $cols column(s)
           """.stripMargin)

      def resultComputed(result: Result[ProtoJson]): F[Unit] =
        logger.info(s"result: $result")
    }

  def statsMonitor[F[_]: Sync]: F[SqlStatsMonitor[F, Fragment]] =
    Ref[F].of(List.empty[SqlStatsMonitor.SqlStats]).map { ref =>
      new SqlStatsMonitor[F, Fragment](ref) {
        def inspect(fragment: Fragment): (String, List[Any]) =
          (fragment.internals.sql, fragment.internals.elements)
      }
    }

}
