// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package skunk

import _root_.skunk.Session
import cats.effect.{ Resource, Sync }

trait SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F]

  final def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]]): Mapping[F] =
    mkMapping(pool, SkunkMonitor.noopMonitor)

}
