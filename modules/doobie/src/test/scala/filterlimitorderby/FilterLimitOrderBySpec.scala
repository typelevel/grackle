// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package filterlimitorderby

import grackle.test.SqlFilterLimitOrderBySpec
import utils.DatabaseSuite

final class FilterLimitOrderBySpec extends DatabaseSuite with SqlFilterLimitOrderBySpec {
  lazy val mapping = FilterLimitOrderByMapping.fromTransactor(xa)
}
