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
package skunk

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import cats.effect.Ref
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
      def queryMapped(query: Query, fragment: AppliedFragment, rows: Int, cols: Int): F[Unit] = ().pure[F]
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
