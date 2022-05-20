// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package demo

import cats.effect.{ Async, ExitCode, IO, IOApp }
import cats.implicits._
import fs2.Stream
import org.http4s.implicits._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.server.staticcontent._

import starwars.StarWarsService

// #server
object Main extends IOApp {
  def run(args: List[String]) =
    DemoServer.stream[IO].compile.drain.as(ExitCode.Success)
}

object DemoServer {
  def stream[F[_]: Async]: Stream[F, Nothing] = {
    val starWarsService = StarWarsService.service[F]

    val httpApp0 = (
      // Routes for static resources, ie. GraphQL Playground
      resourceServiceBuilder[F]("/assets").toRoutes <+>
      // Routes for the Star Wars GraphQL service
      StarWarsService.routes[F](starWarsService)
    ).orNotFound

    val httpApp = Logger.httpApp(true, false)(httpApp0)

    // Spin up the server ...
    for {
      exitCode <- BlazeServerBuilder[F]
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(httpApp)
        .serve
    } yield exitCode
  }.drain
}
// #server
