// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package demo

import cats.effect.Async
import cats.implicits._
import fs2.Stream
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.server.staticcontent.resourceServiceBuilder

// #server
object DemoServer {
  def stream[F[_]: Async](graphQLRoutes: HttpRoutes[F]): Stream[F, Nothing] = {
    val httpApp0 = (
      // Routes for static resources, i.e. GraphQL Playground
      resourceServiceBuilder[F]("/assets").toRoutes <+>
      // GraphQL routes
      graphQLRoutes
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
