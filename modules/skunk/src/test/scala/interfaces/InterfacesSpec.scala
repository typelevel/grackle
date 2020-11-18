// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package interfaces

import utils.DatabaseSuite
import grackle.test.SqlInterfacesSpec

final class InterfacesSpec extends DatabaseSuite with SqlInterfacesSpec {
  lazy val mapping = InterfacesMapping.mkMapping(pool)
}
