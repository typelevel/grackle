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

package effects

import cats.effect.Sync
import cats.implicits._
import fs2.concurrent.SignallingRef

import grackle._
import grackle.syntax._

class ValueEffectMapping[F[_]: Sync](ref: SignallingRef[F, Int]) extends ValueMapping[F] {
  val schema =
    schema"""
      type Query {
        foo: Struct!
      }
      type Struct {
        n: Int!
        s: String!
      }
    """

  val QueryType = schema.ref("Query")
  val StructType = schema.ref("Struct")

  case class Struct(n: Int, s: String)

  val typeMappings = List(
    ObjectMapping(
      tpe = QueryType,
      fieldMappings =
        List(
          // Compute a ValueCursor
          RootEffect.computeCursor("foo")((p, e) =>
            ref.update(_+1).as(
              Result(valueCursor(p, e, Struct(42, "hi")))
            )
          )
        )
    ),
    ValueObjectMapping[Struct](
      tpe = StructType,
      fieldMappings =
        List(
          ValueField("n", _.n),
          ValueField("s", _.s),
        )
    )
  )
}
