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

package compiler

import cats.data.NonEmptyChain
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import Query._
import Value._

final class InputValuesSuite extends CatsEffectSuite {
  test("null value") {
    val query = """
      query {
        field {
          subfield
        }
        field(arg: null) {
          subfield
        }
        field(arg: 23) {
          subfield
        }
      }
    """

    val expected =
      Group(List(
        UntypedSelect("field", None, List(Binding("arg", AbsentValue)), Nil, UntypedSelect("subfield", None, Nil, Nil, Empty)),
        UntypedSelect("field", None, List(Binding("arg", NullValue)), Nil, UntypedSelect("subfield", None, Nil, Nil, Empty)),
        UntypedSelect("field", None, List(Binding("arg", IntValue(23))), Nil, UntypedSelect("subfield", None, Nil, Nil, Empty))
      ))

    val compiled = InputValuesMapping.compiler.compile(query, None)
    //println(compiled)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("list value") {
    val query = """
      query {
        listField(arg: []) {
          subfield
        }
        listField(arg: ["foo", "bar"]) {
          subfield
        }
      }
    """

    val expected =
      Group(List(
        UntypedSelect("listField", None, List(Binding("arg", ListValue(Nil))), Nil,
          UntypedSelect("subfield", None, Nil, Nil, Empty)
        ),
        UntypedSelect("listField", None, List(Binding("arg", ListValue(List(StringValue("foo"),  StringValue("bar"))))), Nil,
          UntypedSelect("subfield", None, Nil, Nil, Empty)
        )
      ))

    val compiled = InputValuesMapping.compiler.compile(query, None)
    //println(compiled)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("input object value") {
    val query = """
      query {
        objectField(arg: { foo: 23, bar: true, baz: "quux" }) {
          subfield
        }
      }
    """

    val expected =
      UntypedSelect("objectField", None,
        List(Binding("arg",
          ObjectValue(List(
            ("foo", IntValue(23)),
            ("bar", BooleanValue(true)),
            ("baz", StringValue("quux")),
            ("defaulted", StringValue("quux")),
            ("nullable", AbsentValue)
          ))
        )),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    //println(compiled)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("invalid input object value") {
    val query = """
      query {
        objectField(arg: { foo: 23, bar: true, baz: "quux", wibble: 10 }) {
          subfield
        }
      }
    """

    val expected = Problem("Unknown field(s) 'wibble' for input object value of type InObj in field 'objectField' of type 'Query'")

    val compiled = InputValuesMapping.compiler.compile(query, None)
    //println(compiled)
    assertEquals(compiled.map(_.query), Result.Failure(NonEmptyChain.one(expected)))
  }
}

object InputValuesMapping extends TestMapping {
  val schema =
    schema"""
      type Query {
        field(arg: Int): Result!
        listField(arg: [String!]!): Result!
        objectField(arg: InObj!): Result!
      }
      type Result {
        subfield: String!
      }
      input InObj {
        foo: Int!
        bar: Boolean!
        baz: String!
        defaulted: String! = "quux"
        nullable: String
      }
    """

  override val selectElaborator = PreserveArgsElaborator
}
