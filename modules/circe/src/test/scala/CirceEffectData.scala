// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
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

package grackle
package circetests

import cats.effect.Sync
import cats.implicits._
import fs2.concurrent.SignallingRef
import io.circe.{Encoder, Json}

import grackle.circe.CirceMapping
import grackle.syntax._

class TestCirceEffectMapping[F[_]: Sync](ref: SignallingRef[F, Int]) extends CirceMapping[F] {
  val schema =
    schema"""
      type Query {
        foo: Struct!
        bar: Struct!
        baz: Struct!
        qux: Struct!
      }
      type Struct {
        n: Int!
        s: String!
      }
    """

  val QueryType = schema.ref("Query")
  val StructType = schema.ref("Struct")

  case class Struct(n: Int, s: String)
  implicit val EncodeStruct: Encoder[Struct] = s =>
    Json.obj(
      "n" -> Json.fromInt(s.n),
      "s" -> Json.fromString(s.s)
    )

  val typeMappings = List(
    ObjectMapping(
      tpe = QueryType,
      fieldMappings =
        List(

          // Compute a CirceCursor
          RootEffect.computeCursor("foo")((p, e) =>
            ref.update(_+1).as(
              Result(circeCursor(p, e,
                Json.obj(
                  "n" -> Json.fromInt(42),
                  "s" -> Json.fromString("hi")
                )
              ))
            )
          ),

          // Compute a Json, let the implementation handle the cursor
          RootEffect.computeJson("bar")((_, _) =>
            ref.update(_+1).as(
              Result(Json.obj(
                "n" -> Json.fromInt(42),
                "s" -> Json.fromString("ho")
              ))
            )
          ),

          // Compute an encodable value, let the implementation handle json and the cursor
          RootEffect.computeEncodable("baz")((_, _) =>
            ref.update(_+1).as(
              Result(Struct(44, "hee"))
            )
          ),

          // Compute a CirceCursor focussed on the root
          RootEffect.computeCursor("qux")((p, e) =>
            ref.update(_+1).as(
              Result(circeCursor(Path(p.rootTpe), e,
                Json.obj(
                  "qux" ->
                    Json.obj(
                      "n" -> Json.fromInt(42),
                      "s" -> Json.fromString("hi")
                    )
                )
              ))
            )
          )
        )
    )
  )
}
