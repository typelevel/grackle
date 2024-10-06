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

import _root_.doobie.util.transactor.Transactor
import cats.effect.Sync
import org.typelevel.log4cats.Logger

trait DoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F]

  def mkMapping[F[_] : Sync](transactor: Transactor[F]): Mapping[F] = {
    val monitor: DoobieMonitor[F] = DoobieMonitor.noopMonitor[F]

    mkMapping(transactor, monitor)
  }

  @deprecated("Use mkMapping instead", "0.2.0")
  def fromTransactor[F[_] : Sync](transactor: Transactor[F]): Mapping[F] =
    mkMapping(transactor)
}

trait LoggedDoobieMappingCompanion {
  def mkMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): Mapping[F]

  def mkMapping[F[_] : Sync : Logger](transactor: Transactor[F]): Mapping[F] = {
    val monitor: DoobieMonitor[F] = DoobieMonitor.loggerMonitor[F](Logger[F])

    mkMapping(transactor, monitor)
  }

  @deprecated("Use mkMapping instead", "0.2.0")
  def fromTransactor[F[_] : Sync : Logger](transactor: Transactor[F]): Mapping[F] =
    mkMapping(transactor)
}
