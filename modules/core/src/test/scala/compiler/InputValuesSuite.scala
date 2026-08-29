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
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.Query._
import grackle.Value._
import grackle.syntax._

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
      Group(
        List(
          UntypedSelect(
            "field",
            Some("one"),
            List(Binding("arg", AbsentValue)),
            Nil,
            UntypedSelect("subfield", None, Nil, Nil, Empty)),
          UntypedSelect(
            "field",
            Some("two"),
            List(Binding("arg", NullValue)),
            Nil,
            UntypedSelect("subfield", None, Nil, Nil, Empty)),
          UntypedSelect(
            "field",
            Some("three"),
            List(Binding("arg", IntValue(23))),
            Nil,
            UntypedSelect("subfield", None, Nil, Nil, Empty))
        ))

    val compiled = InputValuesMapping.compiler.compile(query, None)
    // println(compiled)
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
      Group(
        List(
          UntypedSelect(
            "listField",
            Some("one"),
            List(Binding("arg", ListValue(Nil))),
            Nil,
            UntypedSelect("subfield", None, Nil, Nil, Empty)
          ),
          UntypedSelect(
            "listField",
            Some("two"),
            List(Binding("arg", ListValue(List(StringValue("foo"), StringValue("bar"))))),
            Nil,
            UntypedSelect("subfield", None, Nil, Nil, Empty)
          )
        ))

    val compiled = InputValuesMapping.compiler.compile(query, None)
    // println(compiled)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("single value coerces to a list of size one") {
    val query = """
      query {
        listField(arg: "foo") {
          subfield
        }
      }
    """

    val expected =
      UntypedSelect(
        "listField",
        None,
        List(Binding("arg", ListValue(List(StringValue("foo"))))),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("single value coerces to a nested list") {
    val query = """
      query {
        nestedListField(arg: 1) {
          subfield
        }
      }
    """

    val expected =
      UntypedSelect(
        "nestedListField",
        None,
        List(Binding("arg", ListValue(List(ListValue(List(IntValue(1))))))),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("single input object coerces to a list of size one") {
    val query = """
      query {
        objectListField(arg: { foo: 23, bar: true, baz: "quux" }) {
          subfield
        }
      }
    """

    val expected =
      UntypedSelect(
        "objectListField",
        None,
        List(
          Binding(
            "arg",
            ListValue(
              List(
                ObjectValue(
                  List(
                    ("foo", IntValue(23)),
                    ("bar", BooleanValue(true)),
                    ("baz", StringValue("quux")),
                    ("defaulted", StringValue("quux")),
                    ("nullable", AbsentValue)
                  ))))
          )),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("null is not wrapped in a list") {
    val query = """
      query {
        nullableListField(arg: null) {
          subfield
        }
      }
    """

    val expected =
      UntypedSelect(
        "nullableListField",
        None,
        List(Binding("arg", NullValue)),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("single value of the wrong type is still rejected for a list") {
    val query = """
      query {
        listField(arg: 23) {
          subfield
        }
      }
    """

    val expected =
      Problem("Expected String found '23' for 'arg' in field 'listField' of type 'Query'")

    val compiled = InputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Failure(NonEmptyChain.one(expected)))
  }

  test("an Int literal coerces to a Float") {
    val query = """
      query {
        floatField(arg: 123) {
          subfield
        }
      }
    """

    val expected =
      UntypedSelect(
        "floatField",
        None,
        List(Binding("arg", FloatValue(123.0))),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("an Int literal coerces to a Float inside a list") {
    val query = """
      query {
        floatListField(arg: [1, 2.5]) {
          subfield
        }
      }
    """

    val expected =
      UntypedSelect(
        "floatListField",
        None,
        List(Binding("arg", ListValue(List(FloatValue(1.0), FloatValue(2.5))))),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("an Int default coerces to a Float") {
    val query = """
      query {
        defaultedFloatField {
          subfield
        }
      }
    """

    val expected =
      UntypedSelect(
        "defaultedFloatField",
        None,
        List(Binding("arg", FloatValue(5.0))),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("a Float literal at an Int location is rejected") {
    val query = """
      query {
        field(arg: 1.5) {
          subfield
        }
      }
    """

    val expected =
      Problem("Expected Int found '1.5' for 'arg' in field 'field' of type 'Query'")

    val compiled = InputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Failure(NonEmptyChain.one(expected)))
  }

  test("a String literal at a Float location is rejected") {
    val query = """
      query {
        floatField(arg: "foo") {
          subfield
        }
      }
    """

    val expected =
      Problem("Expected Float found '\"foo\"' for 'arg' in field 'floatField' of type 'Query'")

    val compiled = InputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Failure(NonEmptyChain.one(expected)))
  }

  test("single variable value coerces to a list of size one") {
    val query = """
      query ($arg: [String]!) {
        nullableListField(arg: $arg) {
          subfield
        }
      }
    """

    val variables = json"""{ "arg": "foo" }"""

    val expected =
      UntypedSelect(
        "nullableListField",
        None,
        List(Binding("arg", ListValue(List(StringValue("foo"))))),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, untypedVars = Some(variables))
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("an argument default applies to an absent variable") {
    val query = """
      query ($arg: [String]) {
        defaultedListField(arg: $arg) {
          subfield
        }
      }
    """

    val expected =
      UntypedSelect(
        "defaultedListField",
        None,
        List(Binding("arg", ListValue(List(StringValue("foo"))))),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, untypedVars = Some(json"""{}"""))
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("null variable value is not wrapped in a list") {
    val query = """
      query ($arg: [String]) {
        nullableListField(arg: $arg) {
          subfield
        }
      }
    """

    val variables = json"""{ "arg": null }"""

    val expected =
      UntypedSelect(
        "nullableListField",
        None,
        List(Binding("arg", NullValue)),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, untypedVars = Some(variables))
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
      UntypedSelect(
        "objectField",
        None,
        List(
          Binding(
            "arg",
            ObjectValue(
              List(
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
    // println(compiled)
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

    val expected = Problem(
      "Unknown field(s) 'wibble' for input object value of type InObj in field 'objectField' of type 'Query'")

    val compiled = InputValuesMapping.compiler.compile(query, None)
    // println(compiled)
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
      UntypedSelect(
        "oneOfField",
        None,
        List(
          Binding(
            "arg",
            ObjectValue(
              List(
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

    val expected = Problem(
      "Exactly one key must be specified for oneOf input object OneOfInObj in field 'oneOfField' of type 'Query', but found 'a', 'b'")

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

    val expected = Problem(
      "Value for member field 'a' must be non-null for OneOfInObj in field 'oneOfField' of type 'Query'")

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

    val expected = Problem(
      "Exactly one key must be specified for oneOf input object OneOfInObj in field 'oneOfField' of type 'Query'")

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

    val expected = Problem(
      "Exactly one key must be specified for oneOf input object OneOfInObj in field 'oneOfField' of type 'Query', but found 'a', 'b'")

    val compiled = OneOfInputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Failure(NonEmptyChain.one(expected)))
  }

  test("single value default coerces to a list of size one") {
    val query = """
      query {
        defaultedListField {
          subfield
        }
      }
    """

    val expected =
      UntypedSelect(
        "defaultedListField",
        None,
        List(Binding("arg", ListValue(List(StringValue("foo"))))),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("a supplied value and a default of the same shape agree") {
    val supplied = """
      query {
        defaultedListField(arg: "foo") {
          subfield
        }
      }
    """

    val defaulted = """
      query {
        defaultedListField {
          subfield
        }
      }
    """

    assertEquals(
      InputValuesMapping.compiler.compile(supplied, None).map(_.query),
      InputValuesMapping.compiler.compile(defaulted, None).map(_.query))
  }

  test("single value default of an input object field coerces to a list") {
    val query = """
      query {
        defaultedObjectField(arg: {}) {
          subfield
        }
      }
    """

    val expected =
      UntypedSelect(
        "defaultedObjectField",
        None,
        List(Binding("arg", ObjectValue(List(("xs", ListValue(List(IntValue(1)))))))),
        Nil,
        UntypedSelect("subfield", None, Nil, Nil, Empty)
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    assertEquals(compiled.map(_.query), Result.Success(expected))
  }
}

object InputValuesMapping extends TestMapping {
  val schema =
    schema"""
      type Query {
        field(arg: Int): Result!
        floatField(arg: Float): Result!
        floatListField(arg: [Float]): Result!
        defaultedFloatField(arg: Float = 5): Result!
        listField(arg: [String!]!): Result!
        nullableListField(arg: [String]): Result!
        defaultedListField(arg: [String] = "foo"): Result!
        nestedListField(arg: [[Int]]): Result!
        objectListField(arg: [InObj!]!): Result!
        objectField(arg: InObj!): Result!
        defaultedObjectField(arg: DefObj!): Result!
      }
      type Result {
        subfield: String!
      }
      input DefObj {
        xs: [Int] = 1
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
