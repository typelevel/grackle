// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package filterorderoffsetlimit

import grackle.test.SqlFilterOrderOffsetLimitSpec
import utils.DatabaseSuite

final class FilterOrderOffsetLimitSpec extends DatabaseSuite with SqlFilterOrderOffsetLimitSpec {
  lazy val mapping = FilterOrderOffsetLimitMapping.mkMapping(pool)
}
