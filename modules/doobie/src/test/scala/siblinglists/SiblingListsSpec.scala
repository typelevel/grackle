// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package siblinglists

import grackle.test.SqlSiblingListsSpec
import utils.DatabaseSuite

final class SiblingListsSpec extends DatabaseSuite with SqlSiblingListsSpec {
  lazy val mapping = SiblingListsData.fromTransactor(xa)
}
