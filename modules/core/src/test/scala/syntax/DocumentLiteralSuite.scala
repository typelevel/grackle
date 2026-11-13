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

import grackle.Ast.Document
import grackle.GraphQLParser
import grackle.syntax._

final class DocumentLiteralSuite extends FunSuite {

  // The `doc` interpolator parses at compile time; this checks it agrees with the runtime parser.
  // Both are given exactly the same text, so the `location`s recorded in the AST match too.
  test("doc literal matches the runtime parse of the same text") {
    val text = "query { character(id: 1000) { name } }"

    val literal: Document = doc"query { character(id: 1000) { name } }"

    val parsed = GraphQLParser(GraphQLParser.defaultConfig).parseText(text)

    assertEquals(parsed.toOption, Some(literal))
  }
}
