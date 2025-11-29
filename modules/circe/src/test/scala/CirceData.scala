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

package grackle
package circetests

import cats.effect.IO
import cats.data.OptionT
import io.circe.Json
import io.circe.literal._

import grackle.circe.CirceMapping
import grackle.syntax._

import Query._
import QueryCompiler._

object TestCirceMapping extends CirceMapping[IO] {
  val schema =
    schema"""
      type Query {
        root: Root
      }
      type Root {
        bool: Boolean
        int: Int
        float: Float
        string: String
        id: ID
        choice: Choice
        array: [Int!]
        object: A
        numChildren: Int
        bigDecimal: BigDecimal
        children: [Child!]!
        computed: Int
      }
      enum Choice {
        ONE
        TWO
        THREE
      }
      interface Child {
        id: ID
      }
      type A implements Child {
        id: ID
        aField: Int
      }
      type B implements Child {
        id: ID
        bField: String
      }
      scalar BigDecimal
    """

  val QueryType = schema.ref("Query")
  val RootType = schema.ref("Root")
  val BigDecimalType = schema.ref("BigDecimal")

  val data =
    json"""
      {
        "bool": true,
        "int": 23,
        "float": 1.3,
        "string": "foo",
        "bigDecimal": 1.2,
        "id": "bar",
        "array": [1, 2, 3],
        "choice": "ONE",
        "object": {
          "id": "obj",
          "aField": 27
        },
        "children": [
          {
            "id": "a",
            "aField": 11
          },
          {
            "id": "b",
            "bField": "quux"
          }
        ],
        "hidden": 13
      }
    """

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            CirceField("root", data),
          )
      ),
      ObjectMapping(
        tpe = RootType,
        fieldMappings =
          List(
            CursorField("computed", computeField, List("hidden"))
          )
      ),
      LeafMapping[BigDecimal](BigDecimalType)
    )

  def computeField(c: Cursor): Result[Option[Int]] = {
    (for {
      n <- OptionT(c.fieldAs[Json]("hidden").map(_.asNumber))
      i <- OptionT(Result(n.toInt))
    } yield i+1).value
  }

  override val selectElaborator = SelectElaborator {
    case (RootType, "numChildren", Nil) =>
      Elab.transformChild(_ => Count(Select("children")))
  }
}
