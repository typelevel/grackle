// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle

import grackle.sql.SqlMonitor
import _root_.skunk.AppliedFragment

package object skunk {
  type SkunkMonitor[F[_]] = SqlMonitor[F, AppliedFragment]
}
