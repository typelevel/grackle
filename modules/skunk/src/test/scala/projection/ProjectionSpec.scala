// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package projection

import grackle.test.SqlProjectionSpec
import utils.DatabaseSuite

final class ProjectionSpec extends DatabaseSuite with SqlProjectionSpec {
  lazy val mapping = ProjectionMapping.mkMapping(pool)

  // N.B. these don't work in skunk yet, they're here as a reminder. when it's working pull up
  // the doobie test into the shared spec
  ignore("simple projected queries") {}
  ignore("projected query with nested query") {}
  ignore("projected query with nested filtered queries (0)") {}
  ignore("projected query with nested filtered queries (1)") {}

  // Need at least one non-ignored test to ensure that the ignored
  // tests are reported as such.
  test("dummy") {}

}
