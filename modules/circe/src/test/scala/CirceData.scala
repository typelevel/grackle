// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package circetests

import cats.Id
import cats.implicits._

import edu.gemini.grackle.circe.CirceMapping
import edu.gemini.grackle.syntax._

import Query._
import QueryCompiler._

object TestCirceMapping extends CirceMapping[Id] {
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
        ]
      }
    """

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            CirceRoot("root", data),
          )
      ),
      LeafMapping[BigDecimal](BigDecimalType)
    )

  override val selectElaborator = new SelectElaborator(Map(
    RootType -> {
      case Select("numChildren", Nil, Empty) =>
        Count("numChildren", Select("children", Nil, Empty)).rightIor
    }
  ))
}
