// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all._
import com.comcast.ip4s._
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.{ErrorAction, ErrorHandling, Logger}
import org.http4s.server.staticcontent.resourceServiceBuilder

// #server
object DemoServer {
  def mkServer(graphQLRoutes: HttpRoutes[IO]): Resource[IO, Unit] = {
    val httpApp0 = (
      // Routes for static resources, i.e. GraphQL Playground
      resourceServiceBuilder[IO]("/assets").toRoutes <+>
      // GraphQL routes
      graphQLRoutes
    ).orNotFound

    val httpApp = Logger.httpApp(true, false)(httpApp0)

    val withErrorLogging: HttpApp[IO] = ErrorHandling.Recover.total(
      ErrorAction.log(
        httpApp,
        messageFailureLogAction = errorHandler,
        serviceErrorLogAction = errorHandler))

    // Spin up the server ...
    EmberServerBuilder.default[IO]
      .withHost(ip"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(withErrorLogging)
      .build.void
  }

  def errorHandler(t: Throwable, msg: => String) : IO[Unit] =
    IO.println(msg) >> IO.println(t) >> IO.println(t.printStackTrace())
}
// #server
