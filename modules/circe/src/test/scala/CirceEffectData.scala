// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package circetests

import cats.implicits._
import cats.effect.Sync
import io.circe.Encoder
import io.circe.Json

import edu.gemini.grackle.circe.CirceMapping
import edu.gemini.grackle.syntax._

class TestCirceEffectMapping[F[_]: Sync] extends CirceMapping[F] {
  val schema =
    schema"""
      type Query { 
        foo: Struct!
        bar: Struct!
        baz: Struct!
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
      schema.ref("Query"), 
        List(

          // Compute a CirceCursor
          RootEffect.computeCursor("foo")((_, t, e) =>
            Sync[F].delay(println(s"!!! a side effect! !!!")).as(
              Result(circeCursor(t, e, 
                Json.obj(
                  "n" -> Json.fromInt(42), 
                  "s" -> Json.fromString("hi")
                )
              ))
            )
          ),

          // Compute a Json, let the implementation handle the cursor
          RootEffect.computeJson("bar")((_, _, _) =>
            Sync[F].delay(println(s"!!! a side effect! (2) !!!")).as(
              Result(Json.obj(
                "n" -> Json.fromInt(42), 
                "s" -> Json.fromString("ho")
              ))
            )
          ),

          // Compute an encodable value, let the implementation handle json and the cursor
          RootEffect.computeEncodable("baz")((_, _, _) =>
            Sync[F].delay(println(s"!!! a side effect! (3) !!!")).as(
              Result(Struct(44, "hee"))
            )
          ),

        )
    ),
  )
}
