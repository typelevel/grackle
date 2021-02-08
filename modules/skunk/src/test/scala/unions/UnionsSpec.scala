// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package unions

import grackle.test.SqlUnionSpec
import utils.DatabaseSuite

final class UnionsSpec extends DatabaseSuite with SqlUnionSpec {
  lazy val mapping = UnionsMapping.mkMapping(pool)
}
