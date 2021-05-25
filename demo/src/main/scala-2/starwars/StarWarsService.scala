// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package starwars

import cats.{ Applicative }
import cats.effect.Sync
import cats.implicits._
import io.circe.{ Json, ParsingFailure, parser }
import org.http4s.{ HttpRoutes, InvalidMessageBodyFailure, ParseFailure, QueryParamDecoder }
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl

// #service
trait StarWarsService[F[_]]{
  def runQuery(op: Option[String], vars: Option[Json], query: String): F[Json]
}

object StarWarsService {
  def routes[F[_]: Sync](service: StarWarsService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    implicit val jsonQPDecoder: QueryParamDecoder[Json] = QueryParamDecoder[String].emap { s =>
      parser.parse(s).leftMap { case ParsingFailure(msg, _) => ParseFailure("Invalid variables", msg) }
    }

    object QueryMatcher extends QueryParamDecoderMatcher[String]("query")
    object OperationNameMatcher extends OptionalQueryParamDecoderMatcher[String]("operationName")
    object VariablesMatcher extends OptionalValidatingQueryParamDecoderMatcher[Json]("variables")

    HttpRoutes.of[F] {
      // GraphQL query is embedded in the URI query string when queried via GET
      case GET -> Root / "starwars" :?  QueryMatcher(query) +& OperationNameMatcher(op) +& VariablesMatcher(vars0) =>
        vars0.sequence.fold(
          errors => BadRequest(errors.map(_.sanitized).mkString_("", ",", "")),
          vars =>
            for {
              result <- service.runQuery(op, vars, query)
              resp   <- Ok(result)
            } yield resp
          )

      // GraphQL query is embedded in a Json request body when queried via POST
      case req @ POST -> Root / "starwars" =>
        for {
          body   <- req.as[Json]
          obj    <- body.asObject.liftTo[F](InvalidMessageBodyFailure("Invalid GraphQL query"))
          query  <- obj("query").flatMap(_.asString).liftTo[F](InvalidMessageBodyFailure("Missing query field"))
          op     =  obj("operationName").flatMap(_.asString)
          vars   =  obj("variables")
          result <- service.runQuery(op, vars, query)
          resp   <- Ok(result)
        } yield resp
    }
  }

  def service[F[_]](implicit F: Applicative[F]): StarWarsService[F] =
    new StarWarsService[F]{
      def runQuery(op: Option[String], vars: Option[Json], query: String): F[Json] =
        StarWarsMapping.compileAndRun(query, op, vars).pure[F]
    }
}
// #service
