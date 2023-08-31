// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package circetests

import cats.effect.Sync
import cats.implicits._
import fs2.concurrent.SignallingRef
import io.circe.{Encoder, Json}

import edu.gemini.grackle.circe.CirceMapping
import edu.gemini.grackle.syntax._

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
