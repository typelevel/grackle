// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package demo

import java.util.concurrent._

import cats.effect.{ Blocker, ContextShift, ConcurrentEffect, ExitCode, IO, IOApp, Timer }
import cats.implicits._
import fs2.Stream
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.server.staticcontent._

import starwars.StarWarsService

object Main extends IOApp {
  def run(args: List[String]) =
    DemoServer.stream[IO].compile.drain.as(ExitCode.Success)
}

object DemoServer {
  def stream[F[_]: ConcurrentEffect : ContextShift](implicit T: Timer[F]): Stream[F, Nothing] = {
    val blockingPool = Executors.newFixedThreadPool(4)
    val blocker = Blocker.liftExecutorService(blockingPool)
    val starWarsService = StarWarsService.service[F]

    val httpApp0 = (
      resourceService[F](ResourceService.Config("/assets", blocker)) <+>
      StarWarsService.routes[F](starWarsService)
    ).orNotFound

    val httpApp = Logger.httpApp(true, false)(httpApp0)

    for {
      exitCode <- BlazeServerBuilder[F]
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(httpApp)
        .serve
    } yield exitCode
  }.drain
}
