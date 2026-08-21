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

import io.circe.literal._

/**
 * Conformance test cases for section 4, Introspection.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Introspection
 */
final class IntrospectionSuite extends ConformanceSuite {

  // 4 Introspection
  // https://spec.graphql.org/September2025/#sec-Introspection

  validSchema("an object type which introspection can describe")("""
    type User {
      id: String
      name: String
      birthday: Date
    }

    # Added to complete the example: the `Date` scalar and a query root type.
    scalar Date
    type Query { user: User }
  """)

  yields("the __type meta-field describes a named type", IntrospectionMappings.Site)("""
    {
      __type(name: "User") {
        name
        fields {
          name
          type {
            name
          }
        }
      }
    }
  """)(json"""
    {
      "data": {
        "__type": {
          "name": "User",
          "fields": [
            {
              "name": "id",
              "type": { "name": "String" }
            },
            {
              "name": "name",
              "type": { "name": "String" }
            },
            {
              "name": "birthday",
              "type": { "name": "Date" }
            }
          ]
        }
      }
    }
  """)

  // 4.2.2 The __Type Type
  // https://spec.graphql.org/September2025/#sec-The-__Type-Type

  validSchema("an input object type which introspection can describe")("""
    input Point {
      x: Int
      y: Int
    }

    # Added to complete the example: a query root type.
    type Query { nearest(point: Point): String }
  """)
}
