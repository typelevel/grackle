// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package starwars

import cats.ApplicativeError
import cats.data.Ior
import cats.effect.Sync
import cats.implicits._
import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import edu.gemini.grackle.{ IntrospectionQueryCompiler, IntrospectionQueryInterpreter, QueryInterpreter, SchemaSchema }

// #service
trait StarWarsService[F[_]]{
  def runQuery(op: Option[String], vars: Option[Json], query: String): F[Json]
}

object StarWarsService {
  def routes[F[_]: Sync](service: StarWarsService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    object QueryQueryParamMatcher extends QueryParamDecoderMatcher[String]("query")

    HttpRoutes.of[F] {
      // GraphQL query is embedded in the URI query string when queried via GET
      case GET -> Root / "starwars" :? QueryQueryParamMatcher(query) =>
        for {
          result <- service.runQuery(None, None, query)
          resp   <- Ok(result)
        } yield resp

      // GraphQL query is embedded in a Json request body when queried via POST
      case req @ POST -> Root / "starwars" =>
        for {
          body   <- req.as[Json]
          obj    <- body.asObject.liftTo[F](new RuntimeException("Invalid GraphQL query"))
          op     =  obj("operationName").flatMap(_.asString)
          vars   =  obj("variables")
          query  <- obj("query").flatMap(_.asString).liftTo[F](new RuntimeException("Missing query field"))
          result <- service.runQuery(op, vars, query)
          resp   <- Ok(result)
        } yield resp
    }
  }

  def service[F[_]](implicit F: ApplicativeError[F, Throwable]): StarWarsService[F] =
    new StarWarsService[F]{
      def runQuery(op: Option[String], vars: Option[Json], query: String): F[Json] = {
        if (op == Some("IntrospectionQuery")) {
          // Handle IntrospectionQuery for GraphQL Playground
          IntrospectionQueryCompiler.compile(query, vars) match {
            case Ior.Right(compiledQuery) =>
              IntrospectionQueryInterpreter(StarWarsSchema).run(compiledQuery, SchemaSchema.queryType).pure[F]
            case invalid =>
              QueryInterpreter.mkInvalidResponse(invalid).pure[F]
          }
        } else
          // Handle GraphQL against the model
          StarWarsQueryCompiler.compile(query, vars) match {
            case Ior.Right(compiledQuery) =>
              StarWarsQueryInterpreter.run(compiledQuery, StarWarsSchema.queryType).pure[F]
            case invalid =>
              QueryInterpreter.mkInvalidResponse(invalid).pure[F]
          }
      }
    }
}
// #service
