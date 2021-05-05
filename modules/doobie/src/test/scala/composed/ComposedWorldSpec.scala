// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import utils.DatabaseSuite
import grackle.test.SqlComposedWorldSpec

final class ComposedWorldSpec extends DatabaseSuite with SqlComposedWorldSpec {
  lazy val mapping = ComposedMapping.fromTransactor(xa)
}
