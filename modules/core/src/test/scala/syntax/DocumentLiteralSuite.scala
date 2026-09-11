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

package syntax

import munit.FunSuite

import grackle.GraphQLParser
import grackle.Ast._
import grackle.Ast.OperationDefinition._
import grackle.Ast.OperationType._
import grackle.Ast.Selection._
import grackle.Ast.Value._
import grackle.syntax._

final class DocumentLiteralSuite extends FunSuite {

  test("doc literal matches runtime parse of the same query") {
    val queryText =
      """|query {
         |  character(id: 1000) {
         |    name
         |  }
         |}
         |""".stripMargin

    val literal: Document = doc"""
      query {
        character(id: 1000) {
          name
        }
      }
    """

    val expected =
      Operation(
        Query,
        None,
        Nil,
        Nil,
        List(
          Field(
            None,
            Name("character"),
            List((Name("id"), IntValue(1000))),
            Nil,
            List(
              Field(None, Name("name"), Nil, Nil, Nil)
            )
          )
        )
      )

    assertEquals(literal, List(expected))

    val parsed = GraphQLParser(GraphQLParser.defaultConfig).parseText(queryText).toOption
    assertEquals(parsed, Some(literal))
  }
}
