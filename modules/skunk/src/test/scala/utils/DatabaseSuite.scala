// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package utils

import scala.concurrent.ExecutionContext

import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.Resource
import grackle.test.SqlDatabaseSuite
import natchez.Trace.Implicits.noop
import org.testcontainers.containers.PostgreSQLContainer.POSTGRESQL_PORT
import skunk.Session

trait DatabaseSuite extends SqlDatabaseSuite {

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  // Slow because each usage will open a new socket, but ok for now.
  lazy val pool: Resource[IO, Session[IO]] =
    Session.single[IO](
      host     = container.containerIpAddress,
      port     = container.mappedPort(POSTGRESQL_PORT),
      user     = container.username,
      password = Some(container.password),
      database = container.databaseName,
      // debug    = true,
    )

}
