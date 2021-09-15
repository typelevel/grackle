// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package graph

import utils.DatabaseSuite
import grackle.test.SqlGraphSpec

final class GraphSpec extends DatabaseSuite with SqlGraphSpec {
  lazy val mapping = GraphMapping.mkMapping(pool)
}
