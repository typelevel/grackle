// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package demo

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits._
import com.dimafeng.testcontainers.PostgreSQLContainer
import demo.starwars.StarWarsMapping
import demo.world.WorldMapping
import doobie.hikari.HikariTransactor
import org.flywaydb.core.Flyway
import org.testcontainers.containers.{PostgreSQLContainer => JavaPostgreSQLContainer}
import org.testcontainers.utility.DockerImageName

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    container.use { container =>
      for {
        _ <- dbMigration(container)
        _ <- transactor(container).use { xa =>
          val worldGraphQLRoutes = GraphQLService.routes(
            "world",
            GraphQLService.fromMapping(WorldMapping.mkMappingFromTransactor(xa))
          )
          val starWarsGraphQLRoutes = GraphQLService.routes[IO](
            "starwars",
            GraphQLService.fromGenericIdMapping(StarWarsMapping)
          )
          DemoServer.stream[IO](worldGraphQLRoutes <+> starWarsGraphQLRoutes).compile.drain
        }
      } yield ()
    }.as(ExitCode.Success)
  }

  private val dbName = "test"
  private val dbUser = "test"
  private val dbPassword = "test"
  private val container: Resource[IO, PostgreSQLContainer] = Resource.make(
    IO {
      val containerDef = PostgreSQLContainer.Def(
        DockerImageName.parse(s"${JavaPostgreSQLContainer.IMAGE}:14.5"),
        databaseName = dbName,
        username = dbUser,
        password = dbPassword
      )
      containerDef.start()
    }
  )(c => IO(c.stop()))

  private def dbUri(container: PostgreSQLContainer) =
    s"jdbc:postgresql://${container.host}:${container.mappedPort(JavaPostgreSQLContainer.POSTGRESQL_PORT)}/$dbName"

  private def transactor(container: PostgreSQLContainer): Resource[IO, HikariTransactor[IO]] =
    HikariTransactor.newHikariTransactor[IO](
      "org.postgresql.Driver",
      dbUri(container),
      dbUser,
      dbPassword,
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
    )

  private def dbMigration(container: PostgreSQLContainer): IO[Unit] = IO {
    val flyway = Flyway
      .configure()
      .dataSource(dbUri(container), dbUser, dbPassword)
    flyway.load().migrate()
  }.void
}
