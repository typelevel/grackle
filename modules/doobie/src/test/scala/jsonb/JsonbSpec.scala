// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package jsonb

import utils.DatabaseSuite
import grackle.test.SqlJsonbSpec

final class JsonbSpec extends DatabaseSuite with SqlJsonbSpec {
  lazy val mapping = JsonbMapping.fromTransactor(xa)
}
