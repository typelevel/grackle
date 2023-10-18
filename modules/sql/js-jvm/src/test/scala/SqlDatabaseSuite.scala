// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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
