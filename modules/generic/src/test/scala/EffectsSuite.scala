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
package generic

import cats.effect.{IO, Sync}
import cats.implicits._
import fs2.concurrent.SignallingRef
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

import grackle.syntax._

class GenericEffectMapping[F[_]: Sync](ref: SignallingRef[F, Int]) extends GenericMapping[F] {
  import semiauto._

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
  object Struct {
    implicit val cursorBuilder: CursorBuilder[Struct] =
      deriveObjectCursorBuilder[Struct](StructType)
  }

  val typeMappings = List(
    ObjectMapping(
      tpe = QueryType,
      fieldMappings =
        List(
          // Compute a ValueCursor
          RootEffect.computeCursor("foo")((p, e) =>
            ref.update(_+1).as(
              genericCursor(p, e, Struct(42, "hi"))
            )
          )
        )
    )
  )
}

final class EffectSuite extends CatsEffectSuite {
  test("generic effect") {
    val query = """
      query {
        foo {
          s,
          n
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "foo" : {
            "s" : "hi",
            "n" : 42
          }
        }
      }
    """

    val prg: IO[(Json, Int)] =
      for {
        ref  <- SignallingRef[IO, Int](0)
        map  =  new GenericEffectMapping(ref)
        res  <- map.compileAndRun(query)
        eff  <- ref.get
      } yield (res, eff)

    assertIO(prg, (expected, 1))
  }

  test("generic effect, aliased") {
    val query = """
      query {
        quux:foo {
          s,
          n
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "quux" : {
            "s" : "hi",
            "n" : 42
          }
        }
      }
    """

    val prg: IO[(Json, Int)] =
      for {
        ref  <- SignallingRef[IO, Int](0)
        map  =  new GenericEffectMapping(ref)
        res  <- map.compileAndRun(query)
        eff  <- ref.get
      } yield (res, eff)

    assertIO(prg, (expected, 1))
  }
}
