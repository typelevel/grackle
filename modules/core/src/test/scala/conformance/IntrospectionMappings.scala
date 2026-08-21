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

package conformance

import cats.effect.IO

import grackle._
import grackle.syntax._

/**
 * The mapping for the example of section 4, Introspection.
 *
 * The specification states the response of one introspection request against the type `User`.
 * The mapping holds no data, because an introspection request reads the schema only.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Introspection
 */
object IntrospectionMappings {

  case class User(id: String, name: String, birthday: String)

  object Site extends ValueMapping[IO] {
    val schema =
      schema"""
        scalar Date
        type User {
          id: String
          name: String
          birthday: Date
        }

        # Added to complete the example: a query root type.
        type Query { user: User }
      """

    val QueryType = schema.ref("Query")
    val UserType = schema.ref("User")
    val DateType = schema.ref("Date")

    val typeMappings =
      List(
        ValueObjectMapping[Unit](
          tpe = QueryType,
          fieldMappings = List(ValueField("user", _ => Option.empty[User]))),
        ValueObjectMapping[User](
          tpe = UserType,
          fieldMappings = List(
            ValueField("id", u => Some(u.id)),
            ValueField("name", u => Some(u.name)),
            ValueField("birthday", u => Some(u.birthday))
          )),
        LeafMapping[String](DateType)
      )
  }
}
