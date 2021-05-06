// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import utils.DatabaseSuite
import grackle.test.SqlWorldSpec

final class WorldSpec extends DatabaseSuite with SqlWorldSpec {
  lazy val mapping = WorldMapping.fromTransactor(xa)
}
