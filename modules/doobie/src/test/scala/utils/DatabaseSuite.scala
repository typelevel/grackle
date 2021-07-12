// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package utils

import cats.effect.IO
import doobie.Transactor
import org.typelevel.log4cats.slf4j.Slf4jLogger
import grackle.test.SqlDatabaseSuite
import org.typelevel.log4cats.Logger

trait DatabaseSuite extends SqlDatabaseSuite {

  implicit val log: Logger[IO] = Slf4jLogger.getLogger[IO]

  // lazy vals because the container is not initialised until the test is run

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
