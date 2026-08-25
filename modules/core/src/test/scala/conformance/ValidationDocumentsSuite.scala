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

/**
 * Conformance test cases for the introduction to section 5, and for section 5.1, Documents.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Documents
 */
final class ValidationDocumentsSuite extends ValidationSuite {

  // 5 Validation
  // https://spec.graphql.org/September2025/#sec-Validation

  validSchema("the example schema which the rules of section 5 run against")(
    ValidationSchema.base)

  // 5.1.1 Executable Definitions
  // https://spec.graphql.org/September2025/#sec-Executable-Definitions

  // Grackle drops a type system definition from a request instead of rejecting the document, so
  // the extension does not apply and the field `color` stays undefined. The document is rejected
  // either way.
  invalidQuery("a request must not contain a type system definition or extension")("""
    query getDogName {
      dog {
        name
        color
      }
    }

    extend type Dog {
      color: String
    }
  """)

  // The test case above passes for the reason of the missing field, so this test case isolates
  // rule 5.1.1. The selection set holds no field of the extension, which leaves the extension
  // itself as the only reason to reject the request. Grackle accepts the request.
  invalidQuery("a request which contains a type system extension only".fail)("""
    query getDogName {
      dog {
        name
      }
    }

    extend type Dog {
      color: String
    }
  """)
}
