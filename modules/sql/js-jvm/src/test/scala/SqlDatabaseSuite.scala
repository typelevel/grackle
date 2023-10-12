// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle.sql.test

import scala.concurrent.duration._

import cats.effect.{IO, Resource}
import cats.implicits._
import io.chrisdavenport.whaletail.{Containers, Docker}
import io.chrisdavenport.whaletail.manager._
import munit.CatsEffectSuite
import munit.catseffect._

trait SqlDatabaseSuite extends CatsEffectSuite {
  case class PostgresConnectionInfo(host: String, port: Int) {
    val driverClassName = "org.postgresql.Driver"
    val databaseName = "test"
    val jdbcUrl = s"jdbc:postgresql://$host:$port/$databaseName"
    val username = "test"
    val password = "test"
  }
  object PostgresConnectionInfo {
    val DefaultPort = 5432
  }

  def bindPath(path: String): String =
    buildinfo.BuildInfo.baseDirectory + "/" + path

  val postgresConnectionInfoResource: Resource[IO, PostgresConnectionInfo] =
    Docker.default[IO].flatMap(client =>
      WhaleTailContainer.build(
        client,
        image = "postgres",
        tag = "11.8".some,
        ports = Map(PostgresConnectionInfo.DefaultPort -> None),
        binds = List(Containers.Bind(bindPath("modules/sql/shared/src/test/resources/db/"), "/docker-entrypoint-initdb.d/", "ro")),
        env = Map(
          "POSTGRES_USER" -> "test",
          "POSTGRES_PASSWORD" -> "test",
          "POSTGRES_DB" -> "test"
        ),
        labels = Map.empty
      ).evalTap(
        ReadinessStrategy.checkReadiness(
          client,
          _,
          ReadinessStrategy.LogRegex(".*database system is ready to accept connections.*".r, 2),
          30.seconds
        )
      )
    ).flatMap(container =>
      Resource.eval(
        container.ports.get(PostgresConnectionInfo.DefaultPort).liftTo[IO](new Throwable("Missing Port"))
      )
    ).map {
      case (host, port) => PostgresConnectionInfo(host, port)
    }

  val postgresConnectionInfo: IOFixture[PostgresConnectionInfo] =
    ResourceSuiteLocalFixture("postgresconnectioninfo", postgresConnectionInfoResource)

  override def munitFixtures: Seq[IOFixture[_]] = Seq(postgresConnectionInfo)
}
