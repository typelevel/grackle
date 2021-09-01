// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package tree

import utils.DatabaseSuite
import grackle.test.SqlTreeSpec

final class TreeSpec extends DatabaseSuite with SqlTreeSpec {
  lazy val mapping = TreeMapping.mkMapping(pool)
}
