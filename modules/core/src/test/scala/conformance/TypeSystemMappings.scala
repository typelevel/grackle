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
 * Mappings for the examples of section 3, Type System.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Type-System
 */
object TypeSystemMappings {

  /**
   * The four fields of section 3.6, Field Ordering.
   *
   * The specification numbers the values of each stated result by position, so the values here
   * follow the order of the first example.
   */
  object Ordering extends ValueMapping[IO] {
    val schema = schema"type Query { foo: Int bar: Int baz: Int qux: Int }"

    val QueryType = schema.ref("Query")

    val typeMappings =
      List(
        ValueObjectMapping[Unit](
          tpe = QueryType,
          fieldMappings = List(
            ValueField("foo", _ => Some(1)),
            ValueField("bar", _ => Some(2)),
            ValueField("baz", _ => Some(3)),
            ValueField("qux", _ => Some(4))
          )
        ))
  }
}
