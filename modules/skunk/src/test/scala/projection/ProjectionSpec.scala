// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package projection

import utils.DatabaseSuite
import grackle.test.SqlProjectionSpec

final class ProjectionSpec extends DatabaseSuite with SqlProjectionSpec {
  lazy val mapping = ProjectionMapping.mkMapping(pool)

  // N.B. these doesn't work in skunk yet, they're here as a reminder. when it's working pull up
  // the doobie test into the shared spec
  ignore("simple projected queries") {}
  ignore("projected query with nested filtered queries (0)") {}
}
