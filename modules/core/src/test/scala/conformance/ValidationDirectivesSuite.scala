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

import grackle.syntax._

/**
 * Conformance test cases for section 5.7, Directives.
 *
 * The examples of this section select a field named `field`, which the schema of section 5 does
 * not define. Each test case supplies a schema which does define it.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Validation.Directives
 */
final class ValidationDirectivesSuite extends ValidationSuite {

  // 5.7.2 Directives Are in Valid Locations
  // https://spec.graphql.org/September2025/#sec-Directives-Are-in-Valid-Locations

  invalidQuery("@skip must not appear on an operation definition", LeafFieldSchema)("""
    query @skip(if: $foo) {
      field
    }
  """)

  // 5.7.3 Directives Are Unique per Location
  // https://spec.graphql.org/September2025/#sec-Directives-Are-Unique-per-Location

  invalidQuery("one directive must not appear twice at one location", LeafFieldSchema)("""
    query ($foo: Boolean = true, $bar: Boolean = false) {
      field @skip(if: $foo) @skip(if: $bar)
    }
  """)

  validQuery("one directive can appear once at each of two locations", ObjectFieldSchema)("""
    query ($foo: Boolean = true, $bar: Boolean = false) {
      field @skip(if: $foo) {
        subfieldA
      }
      field @skip(if: $bar) {
        subfieldB
      }
    }
  """)

  // -- Schemas which complete the examples above -----------------------------------------------

  lazy val LeafFieldSchema = schema"type Query { field: Boolean }"

  lazy val ObjectFieldSchema = schema"""
    type Query { field: FieldResult }
    type FieldResult { subfieldA: String subfieldB: String }
  """
}
