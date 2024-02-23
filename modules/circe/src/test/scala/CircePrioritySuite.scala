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

import cats.effect.IO
import io.circe.Json
import io.circe.literal._
import grackle.circe.CirceMapping
import grackle.syntax._
import munit.CatsEffectSuite

object CircePriorityMapping extends CirceMapping[IO] {

  val schema = schema"""
  scalar Foo
  type Monkey {
    name: Foo
  }
  type Barrel {
    monkey: Monkey
  }
  type Query {
    present: Barrel
    fallback: Barrel
  }
  """

  val QueryType  = schema.ref("Query")
  val MonkeyType = schema.ref("Monkey")
  val BarrelType = schema.ref("Barrel")
  val FooType    = schema.ref("Foo") 

  val typeMappings: List[TypeMapping] =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            CirceField("present", json"""{ "monkey": { "name": "Bob" }}"""),
            CirceField("fallback", json"""{ "monkey": {}}"""),
          )
      ),
      ObjectMapping(
        tpe = MonkeyType,
        fieldMappings = 
          List(
            CirceField("name", Json.fromString("Steve"))
          )
      ),
      LeafMapping[String](FooType)
    )    

}

final class CircePrioritySuite extends CatsEffectSuite {

  test("Opaque field should not see explicit mapping.") {

    val query = """
      query {
        present {
          monkey {
            name
          }
        }
        fallback {
          monkey {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "present" : {
            "monkey" : {
              "name" : "Bob"
            }
          },
          "fallback" : {
            "monkey" : {
              "name" : "Steve"
            }
          }
        }
      }
    """

    assertIO(CircePriorityMapping.compileAndRun(query), expected)
  }
}
