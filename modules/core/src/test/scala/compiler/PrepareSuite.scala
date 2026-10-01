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

package compiler

import io.circe.literal.*
import munit.CatsEffectSuite

import grackle.syntax.*

final class PrepareSuite extends CatsEffectSuite {

  // A document with an undefined variable and an unused variable. The undefined variable is
  // always an error. The unused variable is an error only when `reportUnused` is true.
  val mixedVars = """
    query ($used: Int, $unused: Int) {
      foo(n: $undefined)
    }
  """

  test("one prepared document serves both values of reportUnused") {
    val prepared = PrepareMapping.compiler.prepare(mixedVars)
    def viaSplit(reportUnused: Boolean) =
      prepared
        .flatMap(PrepareMapping.compiler.compilePrepared(_, reportUnused = reportUnused))
        .toProblems
        .toList
        .map(_.message)
    def viaCompile(reportUnused: Boolean) =
      PrepareMapping
        .compiler
        .compile(mixedVars, reportUnused = reportUnused)
        .toProblems
        .toList
        .map(_.message)

    val withUnused =
      List(
        "Variable 'undefined' is undefined",
        "Variable 'used' is unused",
        "Variable 'unused' is unused"
      )
    val withoutUnused = List("Variable 'undefined' is undefined")

    assertEquals(viaSplit(true), withUnused)
    assertEquals(viaCompile(true), withUnused)
    assertEquals(viaSplit(false), withoutUnused)
    assertEquals(viaCompile(false), withoutUnused)
  }

  test("a variable value error is reported before a variable usage error") {
    // `$size` is declared nullable and used at a non-null argument, which rule 5.8.5 rejects.
    // No value is supplied for `$n`, which is non-null, so variable coercion fails first.
    val query = """
      query ($n: Int!, $size: Int) {
        bar(n: $n, size: $size)
      }
    """
    val res = PrepareMapping.compiler.compile(query, untypedVars = Some(json"""{}"""))
    val messages = res.toProblems.toList.map(_.message)
    // If `usages` runs before `compileVars`, the first problem names 'size' instead.
    assertEquals(messages.head, "Value of type Int required for 'n' in variable values")
  }
}

object PrepareMapping extends TestMapping {
  val schema =
    schema"""
      type Query {
        foo(n: Int): Int
        bar(n: Int!, size: Int!): Int
      }
    """
}
