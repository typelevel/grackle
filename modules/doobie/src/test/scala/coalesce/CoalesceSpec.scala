// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package coalesce

import utils.DatabaseSuite
import grackle.test.SqlCoalesceSpec
import edu.gemini.grackle.doobie.DoobieStats

final class CoalesceSpec extends DatabaseSuite with SqlCoalesceSpec[DoobieStats] {
  lazy val mapping = CoalesceMapping.fromTransactor(xa)
}
