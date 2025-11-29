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
        one:field {
          subfield
        }
        two:field(arg: null) {
          subfield
        }
        three:field(arg: 23) {
          subfield
        }
      }
    """

    val expected =
      Group(List(
        UntypedSelect("field", Some("one"), List(Binding("arg", AbsentValue)), Nil, UntypedSelect("subfield", None, Nil, Nil, Empty)),
        UntypedSelect("field", Some("two"), List(Binding("arg", NullValue)), Nil, UntypedSelect("subfield", None, Nil, Nil, Empty)),
        UntypedSelect("field", Some("three"), List(Binding("arg", IntValue(23))), Nil, UntypedSelect("subfield", None, Nil, Nil, Empty))
      ))

    val compiled = InputValuesMapping.compiler.compile(query, None)
    //println(compiled)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("list value") {
    val query = """
      query {
        one:listField(arg: []) {
          subfield
        }
        two:listField(arg: ["foo", "bar"]) {
          subfield
        }
      }
    """

    val expected =
      Group(List(
        UntypedSelect("listField", Some("one"), List(Binding("arg", ListValue(Nil))), Nil,
          UntypedSelect("subfield", None, Nil, Nil, Empty)
        ),
        UntypedSelect("listField", Some("two"), List(Binding("arg", ListValue(List(StringValue("foo"),  StringValue("bar"))))), Nil,
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

  test("oneOf input object value") {
    val query = """
      query {
        oneOfField(arg: { a: 42 }) {
          subfield
        }
      }
    """

    val expected =
      UntypedSelect("oneOfField", None,
        List(Binding("arg",
          ObjectValue(List(
            ("a", IntValue(42)),
            ("b", AbsentValue),
            ("c", AbsentValue)
          ))
        )),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = OneOfInputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("invalid oneOf input object value") {
    val query = """
      query {
        oneOfField(arg: { a: 42, b: true }) {
          subfield
        }
      }
    """

    val expected = Problem("Exactly one key must be specified for oneOf input object OneOfInObj in field 'oneOfField' of type 'Query', but found 'a', 'b'")

    val compiled = OneOfInputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Failure(NonEmptyChain.one(expected)))
  }

  test("invalid oneOf input object value with null") {
    val query = """
      query {
        oneOfField(arg: { a: null }) {
          subfield
        }
      }
    """

    val expected = Problem("Value for member field 'a' must be non-null for OneOfInObj in field 'oneOfField' of type 'Query'")

    val compiled = OneOfInputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Failure(NonEmptyChain.one(expected)))
  }

  test("invalid oneOf input object value with absent") {
    val query = """
      query {
        oneOfField(arg: { }) {
          subfield
        }
      }
    """

    val expected = Problem("Exactly one key must be specified for oneOf input object OneOfInObj in field 'oneOfField' of type 'Query'")

    val compiled = OneOfInputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Failure(NonEmptyChain.one(expected)))
  }

  test("invalid oneOf input object value with null and another field") {
    val query = """
      query {
        oneOfField(arg: { a: null, b: true }) {
          subfield
        }
      }
    """

    val expected = Problem("Exactly one key must be specified for oneOf input object OneOfInObj in field 'oneOfField' of type 'Query', but found 'a', 'b'")

    val compiled = OneOfInputValuesMapping.compiler.compile(query, None)
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

object OneOfInputValuesMapping extends TestMapping {
  val schema =
    schema"""
      type Query {
        oneOfField(arg: OneOfInObj!): Result!
      }
      type Result {
        subfield: String!
      }
      input OneOfInObj @oneOf{
        a: Int
        b: Boolean
        c: String
      }
    """

  override val selectElaborator = PreserveArgsElaborator
}
