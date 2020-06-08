// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package utils

import scala.concurrent.ExecutionContext

import cats.effect.{ContextShift, IO}
import cats.tests.CatsSuite
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import doobie.Transactor
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.scalatest.funsuite.AnyFunSuite
import org.testcontainers.containers.BindMode

trait DatabaseSuite extends AnyFunSuite with CatsSuite with ForAllTestContainer {
  override val container: PostgreSQLContainer = PostgreSQLContainer()
  container.container.withClasspathResourceMapping("db", "/docker-entrypoint-initdb.d/", BindMode.READ_ONLY)

  implicit def contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  implicit val log = Slf4jLogger.unsafeCreate[IO]

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
