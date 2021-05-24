// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package utils

import cats.effect.IO
import doobie.Transactor
import grackle.test.SqlDatabaseSuite

trait DatabaseSuite extends SqlDatabaseSuite {

  lazy val xa = {
    import container.{ driverClassName, jdbcUrl, username, password }

    Transactor.fromDriverManager[IO](
      driverClassName,
      jdbcUrl,
      username,
      password
    )
  }
}
