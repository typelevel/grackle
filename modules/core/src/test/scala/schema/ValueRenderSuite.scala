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

package schema

import munit.CatsEffectSuite

import grackle.SchemaRenderer.renderValue
import grackle.Value._

final class ValueRenderSuite extends CatsEffectSuite {
  test("int value renders a negative number") {
    assertEquals(renderValue(IntValue(-23)), "-23")
  }

  test("float value keeps the decimal point") {
    assertEquals(renderValue(FloatValue(1.0)), 1.0.toString())
  }

  test("string value is quoted") {
    assertEquals(renderValue(StringValue("some-id")), "\"some-id\"")
  }

  test("empty string value renders as a pair of quotes") {
    assertEquals(renderValue(StringValue("")), "\"\"")
  }

  test("boolean value renders without quotes") {
    assertEquals(renderValue(BooleanValue(false)), "false")
    assertEquals(renderValue(BooleanValue(true)), "true")
  }

  test("ID value is quoted like a string") {
    assertEquals(renderValue(IDValue("42")), "\"42\"")
  }

  test("enum value renders unquoted") {
    assertEquals(renderValue(EnumValue("NORTH")), "NORTH")
  }

  test("empty list value renders as empty brackets") {
    assertEquals(renderValue(ListValue(Nil)), "[]")
  }

  test("list value renders nested elements") {
    assertEquals(
      renderValue(ListValue(List(IntValue(1), ListValue(List(NullValue))))),
      "[1, [null]]")
  }

  test("empty object value renders as empty braces") {
    assertEquals(renderValue(ObjectValue(Nil)), "{}")
  }

  test("object value renders its fields") {
    assertEquals(
      renderValue(ObjectValue(List(("id", StringValue("a")), ("n", IntValue(2))))),
      "{id: \"a\", n: 2}")
  }

  test("variable reference gets a dollar prefix") {
    assertEquals(renderValue(VariableRef("id")), "$id")
  }

  test("null value renders as null") {
    assertEquals(renderValue(NullValue), "null")
  }

  test("absent value renders as null") {
    assertEquals(renderValue(AbsentValue), "null")
  }
}
