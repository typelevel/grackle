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

import grackle.syntax._

/**
 * Conformance test cases for section 5.2, Operations.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Validation.Operations
 */
final class ValidationOperationsSuite extends ValidationSuite {

  // 5.2.1.1 Operation Type Existence
  // https://spec.graphql.org/September2025/#sec-Operation-Type-Existence

  validSchema("a schema which defines a query root operation type only")("""
    type Query {
      hello: String
    }
  """)

  validQuery("a query operation needs a query root operation type", HelloSchema)("""
    query helloQuery {
      hello
    }
  """)

  invalidQuery("a mutation operation needs a mutation root operation type", HelloSchema)("""
    mutation goodbyeMutation {
      goodbye
    }
  """)

  // 5.2.2.1 Operation Name Uniqueness
  // https://spec.graphql.org/September2025/#sec-Operation-Name-Uniqueness

  validQuery("two operations can have different names")("""
    query getDogName {
      dog {
        name
      }
    }

    query getOwnerName {
      dog {
        owner {
          name
        }
      }
    }
  """)

  invalidQuery("two operations must not share a name")("""
    query getName {
      dog {
        name
      }
    }

    query getName {
      dog {
        owner {
          name
        }
      }
    }
  """)

  invalidQuery("two operations of different types must not share a name")("""
    query dogOperation {
      dog {
        name
      }
    }

    mutation dogOperation {
      mutateDog {
        id
      }
    }
  """)

  // 5.2.3.1 Lone Anonymous Operation
  // https://spec.graphql.org/September2025/#sec-Lone-Anonymous-Operation

  validQuery("a document can hold one anonymous operation")("""
    {
      dog {
        name
      }
    }
  """)

  invalidQuery("an anonymous operation must be the only operation")("""
    {
      dog {
        name
      }
    }

    query getName {
      dog {
        owner {
          name
        }
      }
    }
  """)

  // 5.2.4.1 Single Root Field
  // https://spec.graphql.org/September2025/#sec-Single-Root-Field

  validQuery("a subscription operation can select one root field")("""
    subscription sub {
      newMessage {
        body
        sender
      }
    }
  """)

  validQuery("a fragment can supply the one root field of a subscription")("""
    subscription sub {
      ...newMessageFields
    }

    fragment newMessageFields on Subscription {
      newMessage {
        body
        sender
      }
    }
  """)

  invalidQuery("a subscription operation must not select two root fields".fail)("""
    subscription sub {
      newMessage {
        body
        sender
      }
      disallowedSecondRootField
    }
  """)

  invalidQuery("a fragment must not add a second root field to a subscription".fail)("""
    subscription sub {
      ...multipleSubscriptions
    }

    fragment multipleSubscriptions on Subscription {
      newMessage {
        body
        sender
      }
      disallowedSecondRootField
    }
  """)

  invalidQuery(
    "@skip and @include must not appear on the root selection set of a subscription".fail,
    vars = json"""{"bool": true}""")("""
    subscription requiredRuntimeValidation($bool: Boolean!) {
      newMessage @include(if: $bool) {
        body
        sender
      }
      disallowedSecondRootField @skip(if: $bool)
    }
  """)

  invalidQuery("the one root field of a subscription must not be an introspection field".fail)(
    """
    subscription sub {
      __typename
    }
  """)

  // -- Schemas which complete the examples above -----------------------------------------------

  lazy val HelloSchema = schema"type Query { hello: String }"
}
