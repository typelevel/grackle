// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
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
