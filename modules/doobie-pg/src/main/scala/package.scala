// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.doobie

import grackle.sql.SqlMonitor
import _root_.doobie.Fragment

package object postgres {
  type DoobieMonitor[F[_]] = SqlMonitor[F, Fragment]
}
