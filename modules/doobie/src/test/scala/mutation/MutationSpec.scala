// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package mutation

import utils.DatabaseSuite
import grackle.test.SqlMutationSpec

final class MutationSpec extends DatabaseSuite with SqlMutationSpec {
  lazy val mapping = MutationMapping.fromTransactor(xa)
}
