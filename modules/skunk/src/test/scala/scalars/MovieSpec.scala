// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package scalars

import utils.DatabaseSuite
import grackle.test.SqlMovieSpec

final class MovieSpec extends DatabaseSuite with SqlMovieSpec {
  lazy val mapping = MovieMapping.mkMapping(pool)

  // N.B. this doesn't work in skunk yet, it's here as a reminder. when it's working pull up
  // the doobie test into the shared spec
  ignore("query with arrays") {}

}
