// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import edu.gemini.grackle.sql.SqlMonitor
import _root_.doobie.Fragment

package object doobie {
  type DoobieMonitor[F[_]] = SqlMonitor[F, Fragment]
}