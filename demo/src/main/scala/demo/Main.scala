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

package demo

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all._
import demo.starwars.{StarWarsData, StarWarsMapping}
import demo.world.WorldMapping
import doobie.hikari.HikariTransactor
import io.chrisdavenport.whaletail.Docker
import io.chrisdavenport.whaletail.manager._
import org.flywaydb.core.Flyway

// #main
object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    DBSetup.run { xa =>
      val worldGraphQLRoutes = GraphQLService.routes(
        "world",
        GraphQLService.fromMapping(WorldMapping.mkMappingFromTransactor(xa))
      )
      val starWarsGraphQLRoutes = GraphQLService.routes[IO](
        "starwars",
        GraphQLService.fromMapping(new StarWarsMapping[IO] with StarWarsData[IO])
      )
      DemoServer.resource(worldGraphQLRoutes <+> starWarsGraphQLRoutes)
    }
  }
}
// #main

object DBSetup {
  def run(body: HikariTransactor[IO] => Resource[IO, Unit]): IO[Nothing] = 
    container.evalTap(dbMigration(_)).flatMap(transactor(_)).flatMap(body).useForever

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

  val container: Resource[IO, PostgresConnectionInfo] = Docker.default[IO].flatMap(client =>
    WhaleTailContainer.build(
      client,
      image = "postgres",
      tag = "11.8".some,
      ports = Map(PostgresConnectionInfo.DefaultPort -> None),
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

  def transactor(connInfo: PostgresConnectionInfo): Resource[IO, HikariTransactor[IO]] = {
    import connInfo._
    HikariTransactor.newHikariTransactor[IO](
      driverClassName,
      jdbcUrl,
      username,
      password,
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
    )
  }

  def dbMigration(connInfo: PostgresConnectionInfo): IO[Unit] = {
    import connInfo._
    IO.blocking {
      val flyway = Flyway
        .configure()
        .dataSource(jdbcUrl, username, password)
      flyway.load().migrate()
    }.void
  }
}
