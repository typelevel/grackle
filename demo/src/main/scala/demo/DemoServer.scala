// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package demo

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all._
import com.comcast.ip4s._
import org.http4s.HttpRoutes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.server.staticcontent.resourceServiceBuilder

// #server
object DemoServer {
  def resource(graphQLRoutes: HttpRoutes[IO]): Resource[IO, Unit] = {
    val httpApp0 = (
      // Routes for static resources, i.e. GraphQL Playground
      resourceServiceBuilder[IO]("/assets").toRoutes <+>
      // GraphQL routes
      graphQLRoutes
    ).orNotFound

    val httpApp = Logger.httpApp(true, false)(httpApp0)

    // Spin up the server ...
    EmberServerBuilder.default[IO]
      .withHost(ip"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(httpApp)
      .build.void
  }
}
// #server
