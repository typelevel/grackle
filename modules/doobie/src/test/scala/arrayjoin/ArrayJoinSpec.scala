// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package arrayjoin

import grackle.test.SqlArrayJoinSpec
import utils.DatabaseSuite

final class ArrayJoinSpec extends DatabaseSuite with SqlArrayJoinSpec {
  lazy val mapping = ArrayJoinData.fromTransactor(xa)
}
