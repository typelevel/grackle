// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package circetests

import cats.Id
import io.circe.literal.JsonStringContext

import edu.gemini.grackle.circe.CirceMapping

object TestCirceMapping extends CirceMapping[Id] {
  val schema =
    Schema(
      """
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
          arrary: [Int!]
          object: A
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
      """
    ).right.get

  val QueryType = schema.ref("Query")

  val data =
    json"""
      {
        "bool": true,
        "int": 23,
        "float": 1.3,
        "string": "foo",
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
      )
    )
}
