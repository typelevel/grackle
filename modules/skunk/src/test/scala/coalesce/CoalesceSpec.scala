// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package coalesce

import utils.DatabaseSuite
import edu.gemini.grackle.skunk.SkunkStats
import grackle.test.SqlCoalesceSpec

final class CoalesceSpec extends DatabaseSuite with SqlCoalesceSpec[SkunkStats] {
  lazy val mapping = CoalesceMapping.fromSessionPool(pool)
}
