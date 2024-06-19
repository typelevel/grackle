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

package demo.world

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import cats.effect.{Async, Resource}
import cats.syntax.all._
import doobie.hikari.HikariTransactor
import io.chrisdavenport.whaletail.{Containers, Docker}
import io.chrisdavenport.whaletail.manager._

object WorldData {
  def mkContainer[F[_]: Async]: Resource[F, PostgresConnectionInfo] =
    Docker.default[F].flatMap(client =>
      WhaleTailContainer.build(
        client,
        image = "postgres",
        tag = "11.8".some,
        ports = Map(PostgresConnectionInfo.DefaultPort -> None),
        binds = List(Containers.Bind(bindPath("demo/src/main/resources/db/"), "/docker-entrypoint-initdb.d/", "ro")),
        env = Map(
          "POSTGRES_USER"     -> "test",
          "POSTGRES_PASSWORD" -> "test",
          "POSTGRES_DB"       -> "test"
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
        container.ports.get(PostgresConnectionInfo.DefaultPort).liftTo[F](new Throwable("Missing Port"))
      )
    ).map {
      case (host, port) => PostgresConnectionInfo(host, port)
    }

  def mkTransactor[F[_]: Async](connInfo: PostgresConnectionInfo): Resource[F, HikariTransactor[F]] = {
    import connInfo._
    HikariTransactor.newHikariTransactor[F](
      driverClassName,
      jdbcUrl,
      username,
      password,
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
    )
  }

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
}
