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

import io.circe.Json
import io.circe.literal._
import munit.{CatsEffectSuite, Location}

import grackle._
import grackle.ScalarType._
import grackle.Value._
import grackle.syntax._

/**
 * Tests for rule 5.8.5, All Variable Usages Are Allowed.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-All-Variable-Usages-Are-Allowed
 */
final class VariableUsageSuite extends CatsEffectSuite {

  test("a variable of the same type is allowed") {
    assertAllowed(
      """query ($v: String!) { scalarField(arg: $v) { subfield } }""",
      json"""{"v": "a"}""")
  }

  test("a non-null variable is allowed at a nullable location") {
    assertAllowed(
      """query ($v: String!) { nullableScalarField(arg: $v) { subfield } }""",
      json"""{"v": "a"}""")
  }

  test("a nullable variable is rejected at a non-null location") {
    assertRejected(
      """query ($v: String) { scalarField(arg: $v) { subfield } }""",
      "Variable '$v' of type 'String' is not compatible with argument 'arg' of type 'String!' in field 'scalarField' of type 'Query'"
    )
  }

  test("a nullable variable with a non-null default is allowed at a non-null location") {
    assertAllowed("""query ($v: String = "d") { scalarField(arg: $v) { subfield } }""")
  }

  test("a nullable variable with a null default is rejected at a non-null location") {
    assertRejected(
      """query ($v: String = null) { scalarField(arg: $v) { subfield } }""",
      "Variable '$v' of type 'String' is not compatible with argument 'arg' of type 'String!' in field 'scalarField' of type 'Query'"
    )
  }

  test("a variable of a different named type is rejected") {
    assertRejected(
      """query ($v: Int!) { scalarField(arg: $v) { subfield } }""",
      "Variable '$v' of type 'Int!' is not compatible with argument 'arg' of type 'String!' in field 'scalarField' of type 'Query'",
      json"""{"v": 1}"""
    )
  }

  test("a non-list variable is rejected at a list location") {
    assertRejected(
      """query ($v: String!) { listField(arg: $v) { subfield } }""",
      "Variable '$v' of type 'String!' is not compatible with argument 'arg' of type '[String!]!' in field 'listField' of type 'Query'",
      json"""{"v": "a"}"""
    )
  }

  test("a list variable is rejected at a non-list location") {
    assertRejected(
      """query ($v: [String!]!) { scalarField(arg: $v) { subfield } }""",
      "Variable '$v' of type '[String!]!' is not compatible with argument 'arg' of type 'String!' in field 'scalarField' of type 'Query'",
      json"""{"v": ["a"]}"""
    )
  }

  test("a nullable item is rejected where a non-null item is expected") {
    assertRejected(
      """query ($v: [String]!) { listField(arg: $v) { subfield } }""",
      "Variable '$v' of type '[String]!' is not compatible with argument 'arg' of type '[String!]!' in field 'listField' of type 'Query'",
      json"""{"v": ["a"]}"""
    )
  }

  test("a non-null item is allowed where a nullable item is expected") {
    assertAllowed(
      """query ($v: [String!]!) { nullableItemListField(arg: $v) { subfield } }""",
      json"""{"v": ["a"]}""")
  }

  test("nested list types are compared item by item") {
    assertAllowed(
      """query ($v: [[Int]]!) { nestedListField(arg: $v) { subfield } }""",
      json"""{"v": [[1]]}""")

    assertRejected(
      """query ($v: [Int]!) { nestedListField(arg: $v) { subfield } }""",
      "Variable '$v' of type '[Int]!' is not compatible with argument 'arg' of type '[[Int]]!' in field 'nestedListField' of type 'Query'",
      json"""{"v": [1]}"""
    )
  }

  test("a variable inside a list value is checked against the item type") {
    assertAllowed(
      """query ($v: String!) { listField(arg: [$v]) { subfield } }""",
      json"""{"v": "a"}""")

    assertRejected(
      """query ($v: String) { listField(arg: [$v]) { subfield } }""",
      "Variable '$v' of type 'String' is not compatible with an item of argument 'arg' of type 'String!' in field 'listField' of type 'Query'"
    )
  }

  test("a variable inside an input object is checked against the field type") {
    assertAllowed(
      """query ($v: String!) { objectField(arg: { required: $v }) { subfield } }""",
      json"""{"v": "a"}""")

    assertRejected(
      """query ($v: String) { objectField(arg: { required: $v }) { subfield } }""",
      "Variable '$v' of type 'String' is not compatible with input field 'required' of type 'String!' in field 'objectField' of type 'Query'"
    )
  }

  test("a variable in a directive argument is checked") {
    assertAllowed(
      """query ($v: Boolean!) { scalarField(arg: "a") @skip(if: $v) { subfield } }""",
      json"""{"v": true}""")

    // A value is supplied so that argument coercion succeeds and the usage rule is reached.
    assertRejected(
      """query ($v: Boolean) { scalarField(arg: "a") @skip(if: $v) { subfield } }""",
      "Variable '$v' of type 'Boolean' is not compatible with argument 'if' of type 'Boolean!' in directive 'skip'",
      json"""{"v": true}"""
    )
  }

  test("a variable usage inside a fragment is checked") {
    assertRejected(
      """
        query ($v: String) { ...frag }
        fragment frag on Query { scalarField(arg: $v) { subfield } }
      """,
      "Variable '$v' of type 'String' is not compatible with argument 'arg' of type 'String!' in field 'scalarField' of type 'Query'"
    )
  }

  test("a variable usage inside an inline fragment is checked") {
    assertRejected(
      """query ($v: String) { ... on Query { scalarField(arg: $v) { subfield } } }""",
      "Variable '$v' of type 'String' is not compatible with argument 'arg' of type 'String!' in field 'scalarField' of type 'Query'"
    )
  }

  test("a fragment is validated only against the operations which can reach it") {
    val compiled =
      VariableUsageMapping
        .compiler
        .compile(
          """
            query A($v: String) { ...frag }
            query B($v: [String]) { nullableListField(arg: $v) { subfield } }
            fragment frag on Query { nullableScalarField(arg: $v) { subfield } }
          """,
          name = Some("A"),
          untypedVars = Some(json"""{}""")
        )

    assert(compiled.hasValue, compiled.toString)
  }

  test("a variable in an introspection meta-field argument is checked") {
    val allowed =
      IntrospectionUsageMapping
        .compiler
        .compile(
          """query ($v: String!) { __type(name: $v) { name } }""",
          untypedVars = Some(json"""{"v": "Query"}"""))

    assert(allowed.hasValue, allowed.toString)

    assertRejected(
      """query ($v: String) { __type(name: $v) { name } }""",
      "Variable '$v' of type 'String' is not compatible with argument 'name' of type 'String!' in field '__type' of type 'Query'"
    )
  }

  test("a variable below an introspection meta-field is checked") {
    val allowed =
      IntrospectionUsageMapping
        .compiler
        .compile(
          """query ($v: Boolean!) { __schema { types { fields(includeDeprecated: $v) { name } } } }""",
          untypedVars = Some(json"""{"v": true}"""))

    assert(allowed.hasValue, allowed.toString)

    assertRejected(
      """query ($v: [Boolean!]!) { __schema { types { fields(includeDeprecated: $v) { name } } } }""",
      "Variable '$v' of type '[Boolean!]!' is not compatible with argument 'includeDeprecated' of type 'Boolean!' in field 'fields' of type '__Type'",
      json"""{"v": [true]}"""
    )
  }

  test("a fragment spread twice yields one problem per usage") {
    val compiled =
      compile(
        """
          query ($v: String) { a: scalarField(arg: "a") { ...frag } b: scalarField(arg: "b") { ...frag } }
          fragment frag on Result { subfieldWithArg(arg: $v) }
        """,
        json"""{}"""
      )

    assertEquals(compiled.toProblems.size, 1L)
  }

  test("an input type is a subtype of the same type, and of its nullable form only") {
    assert(StringType <:< StringType)
    assert(StringType <:< NullableType(StringType))
    assert(!(NullableType(StringType) <:< StringType))
    assert(!(IntType <:< StringType))
  }

  test("list types are subtypes item by item") {
    assert(ListType(StringType) <:< ListType(NullableType(StringType)))
    assert(!(ListType(NullableType(StringType)) <:< ListType(StringType)))
    assert(!(StringType <:< ListType(StringType)))
    assert(!(ListType(StringType) <:< StringType))
    assert(ListType(ListType(StringType)) <:< ListType(ListType(StringType)))
    assert(!(ListType(StringType) <:< ListType(ListType(StringType))))
  }

  test("a nullable variable is allowed where the argument has a default") {
    assertAllowed("""query ($v: String) { defaultedField(arg: $v) { subfield } }""")
  }

  test("a nullable variable is allowed where the input field has a default") {
    assertAllowed(
      """query ($v: String) { objectField(arg: { required: "a", defaulted: $v }) { subfield } }""")
  }

  test("a nullable variable needs a default at a non-null location") {
    val nullable = NullableType(StringType)

    assert(!VariableUsage.isVariableUsageAllowed(varDef(nullable, None), StringType, false))
    assert(VariableUsage.isVariableUsageAllowed(varDef(nullable, None), StringType, true))
    assert(
      VariableUsage
        .isVariableUsageAllowed(varDef(nullable, Some(StringValue("x"))), StringType, false))
    assert(
      !VariableUsage
        .isVariableUsageAllowed(varDef(nullable, Some(NullValue)), StringType, false))
  }

  test("a default does not make incompatible types compatible") {
    val nullable = NullableType(IntType)

    assert(!VariableUsage.isVariableUsageAllowed(varDef(nullable, None), StringType, true))
  }

  def varDef(tpe: Type, default: Option[Value]): InputValue =
    InputValue("v", None, tpe, default, Nil)

  def compile(query: String, vars: Json = json"""{}"""): Result[Query] =
    VariableUsageMapping.compiler.compile(query, untypedVars = Some(vars)).map(_.query)

  def assertAllowed(
      query: String,
      vars: Json = json"""{}"""
  )(implicit loc: Location): Unit = {
    val compiled = compile(query, vars)
    assert(compiled.hasValue, compiled.toString)
  }

  def assertRejected(
      query: String,
      message: String,
      vars: Json = json"""{}"""
  )(implicit loc: Location): Unit =
    assertEquals(compile(query, vars), Result.failure(message))
}

object VariableUsageMapping extends TestMapping {
  val schema =
    schema"""
      type Query {
        scalarField(arg: String!): Result!
        nullableScalarField(arg: String): Result!
        defaultedField(arg: String! = "x"): Result!
        listField(arg: [String!]!): Result!
        nullableListField(arg: [String]): Result!
        nullableItemListField(arg: [String]!): Result!
        nestedListField(arg: [[Int]]!): Result!
        objectField(arg: InObj!): Result!
      }
      type Result {
        subfield: String!
        subfieldWithArg(arg: String!): String!
      }
      input InObj {
        required: String!
        optional: String
        defaulted: String! = "x"
      }
    """

  override val selectElaborator = PreserveArgsElaborator
}

/**
 * The same schema with the default elaborator.
 *
 * `PreserveArgsElaborator` cannot elaborate an introspection selection, so the introspection
 * tests use this mapping for the queries which must compile.
 */
object IntrospectionUsageMapping extends TestMapping {
  val schema = VariableUsageMapping.schema
}
