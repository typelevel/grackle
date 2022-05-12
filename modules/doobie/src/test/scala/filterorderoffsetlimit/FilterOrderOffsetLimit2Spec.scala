// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package filterorderoffsetlimit

import grackle.test.SqlFilterOrderOffsetLimit2Spec
import utils.DatabaseSuite

final class FilterOrderOffsetLimit2Spec extends DatabaseSuite with SqlFilterOrderOffsetLimit2Spec {
  lazy val mapping = FilterOrderOffsetLimit2Mapping.fromTransactor(xa)
}
