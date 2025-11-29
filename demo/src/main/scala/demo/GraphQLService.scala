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

import cats.effect.Concurrent
import cats.syntax.all._
import grackle.Mapping
import io.circe.{Json, ParsingFailure, parser}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, InvalidMessageBodyFailure, ParseFailure, QueryParamDecoder}

// #service
object GraphQLService {
  def mkRoutes[F[_]: Concurrent](prefix: String)(mapping: Mapping[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    implicit val jsonQPDecoder: QueryParamDecoder[Json] =
      QueryParamDecoder[String].emap { s =>
        parser.parse(s).leftMap {
          case ParsingFailure(msg, _) => ParseFailure("Invalid variables", msg)
        }
      }

    object QueryMatcher
      extends QueryParamDecoderMatcher[String]("query")
    object OperationNameMatcher
      extends OptionalQueryParamDecoderMatcher[String]("operationName")
    object VariablesMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[Json]("variables")

    HttpRoutes.of[F] {
      // GraphQL query is embedded in the URI query string when queried via GET
      case GET -> Root / `prefix` :? 
             QueryMatcher(query) +& OperationNameMatcher(op) +& VariablesMatcher(vars0) =>
        vars0.sequence.fold(
          errors => BadRequest(errors.map(_.sanitized).mkString_("", ",", "")),
          vars =>
            for {
              result <- mapping.compileAndRun(query, op, vars)
              resp   <- Ok(result)
            } yield resp
        )

      // GraphQL query is embedded in a Json request body when queried via POST
      case req @ POST -> Root / `prefix` =>
        for {
          body   <- req.as[Json]
          obj    <- body.asObject.liftTo[F](
                      InvalidMessageBodyFailure("Invalid GraphQL query")
                    )
          query  <- obj("query").flatMap(_.asString).liftTo[F](
                      InvalidMessageBodyFailure("Missing query field")
                    )
          op     =  obj("operationName").flatMap(_.asString)
          vars   =  obj("variables")
          result <- mapping.compileAndRun(query, op, vars)
          resp   <- Ok(result)
        } yield resp
    }
  }
}
// #service
