// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package unions

import utils.DatabaseSuite
import grackle.test.SqlUnionSpec

final class UnionsSpec extends DatabaseSuite with SqlUnionSpec {
  lazy val mapping = UnionsMapping.fromTransactor(xa)
}
