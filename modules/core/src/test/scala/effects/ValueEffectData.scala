// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package effects

import cats.effect.Sync
import cats.implicits._
import fs2.concurrent.SignallingRef

import edu.gemini.grackle._
import edu.gemini.grackle.syntax._

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
          RootEffect.computeCursor("foo")((_, p, e) =>
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
