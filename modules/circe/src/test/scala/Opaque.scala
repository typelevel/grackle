// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package circetests

import cats.effect.IO
import io.circe.Json
import io.circe.literal._
import edu.gemini.grackle.circe.CirceMapping
import edu.gemini.grackle.syntax._
import munit.CatsEffectSuite

object OpaqueMapping extends CirceMapping[IO] {

  val schema = schema"""
  scalar Foo
  type Monkey {
    name: Foo
  }
  type Barrel {
    monkey: Monkey
  }
  type Query {
    opaque: Barrel
    notOpaque: Barrel
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
            CirceField("opaque", json"""{ "monkey": { "name": "Bob" }}""", opaque = true),
            CirceField("notOpaque", json"""{ "monkey": { "name": "Bob" }}"""),
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

final class OpaqueSuite extends CatsEffectSuite {

  test("Opaque field should not see explicit mapping.") {

    val query = """
      query {
        opaque {
          monkey {
            name
          }
        }
        notOpaque {
          monkey {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "opaque" : {
            "monkey" : {
              "name" : "Bob"
            }
          },
          "notOpaque" : {
            "monkey" : {
              "name" : "Steve"
            }
          }
        }
      }
    """

    assertIO(OpaqueMapping.compileAndRunOne(query), expected)

  }
}
