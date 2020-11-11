// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package utils

import cats.effect.{ContextShift, IO}
import cats.effect.Resource
import cats.tests.CatsSuite
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import natchez.Trace.Implicits.noop
import org.scalatest.funsuite.AnyFunSuite
import org.testcontainers.containers.BindMode
import org.testcontainers.containers.PostgreSQLContainer.POSTGRESQL_PORT
import scala.concurrent.ExecutionContext
import skunk.Session

trait DatabaseSuite extends AnyFunSuite with CatsSuite with ForAllTestContainer {

  override val container: PostgreSQLContainer = {
    val c = PostgreSQLContainer()
    c.container.withClasspathResourceMapping("db", "/docker-entrypoint-initdb.d/", BindMode.READ_ONLY)
    c
  }

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
