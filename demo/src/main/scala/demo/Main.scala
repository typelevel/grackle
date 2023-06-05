// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package demo

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits._
import demo.starwars.{StarWarsData, StarWarsMapping}
import demo.world.WorldMapping
import doobie.hikari.HikariTransactor
import io.chrisdavenport.whaletail.Docker
import io.chrisdavenport.whaletail.manager._
import org.flywaydb.core.Flyway

// #main
object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    container.use { connInfo =>
      for {
        _ <- dbMigration(connInfo)
        _ <- transactor(connInfo).use { xa =>
          val worldGraphQLRoutes = GraphQLService.routes(
            "world",
            GraphQLService.fromMapping(WorldMapping.mkMappingFromTransactor(xa))
          )
          val starWarsGraphQLRoutes = GraphQLService.routes[IO](
            "starwars",
            GraphQLService.fromMapping(new StarWarsMapping[IO] with StarWarsData[IO])
          )
          DemoServer.stream[IO](worldGraphQLRoutes <+> starWarsGraphQLRoutes).compile.drain
        }
      } yield ()
    }.as(ExitCode.Success)
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
    IO {
      val flyway = Flyway
        .configure()
        .dataSource(jdbcUrl, username, password)
      flyway.load().migrate()
    }.void
  }
}
// #main
