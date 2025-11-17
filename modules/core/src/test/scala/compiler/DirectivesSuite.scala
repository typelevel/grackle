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

import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import Ast.DirectiveLocation._
import Query._

final class DirectivesSuite extends CatsEffectSuite {
  val schemaParser = SchemaParser(GraphQLParser(GraphQLParser.defaultConfig))

  def testDirectiveDefs(s: Schema): List[DirectiveDef] =
    s.directives.filter {
      case DirectiveDef("skip"|"include"|"deprecated"|"oneOf", _, _, _, _) => false
      case _ => true
    }

  test("Simple directive definition") {
    val expected = DirectiveDef("foo", None, Nil, false, List(FIELD))
    val schema =
      Schema("""
        type Query {
          foo: Int
        }

        directive @foo on FIELD
      """)

    assertEquals(schema.map(testDirectiveDefs), List(expected).success)
  }

  test("Directive definition with description") {
    val expected = DirectiveDef("foo", Some("A directive"), Nil, false, List(FIELD))
    val schema =
      Schema("""
        type Query {
          foo: Int
        }

        "A directive"
        directive @foo on FIELD
      """)

    assertEquals(schema.map(testDirectiveDefs), List(expected).success)
  }

  test("Directive definition with multiple locations (1)") {
    val expected = DirectiveDef("foo", None, Nil, false, List(FIELD, FRAGMENT_SPREAD, INLINE_FRAGMENT))
    val schema =
      Schema("""
        type Query {
          foo: Int
        }

        directive @foo on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
      """)

    assertEquals(schema.map(testDirectiveDefs), List(expected).success)
  }

  test("Directive definition with multiple locations (2)") {
    val expected = DirectiveDef("foo", None, Nil, false, List(FIELD, FRAGMENT_SPREAD, INLINE_FRAGMENT))
    val schema =
      Schema("""
        type Query {
          foo: Int
        }

        directive @foo on | FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
      """)

    assertEquals(schema.map(testDirectiveDefs), List(expected).success)
  }

  test("Directive definition with repeatable") {
    val expected = DirectiveDef("foo", None, Nil, true, List(FIELD))
    val schema =
      Schema("""
        type Query {
          foo: Int
        }

        directive @foo repeatable on FIELD
      """)

    assertEquals(schema.map(testDirectiveDefs), List(expected).success)
  }

  test("Directive definition with arguments (1)") {
    val expected =
      DirectiveDef(
        "foo",
        None,
        List(InputValue("arg", None, ScalarType.StringType, None, Nil)),
        false,
        List(FIELD)
      )

    val schema =
      Schema("""
        type Query {
          foo: Int
        }

        directive @foo(arg: String!) on FIELD
      """)

    assertEquals(schema.map(testDirectiveDefs), List(expected).success)
  }

  test("Directive definition with arguments (2)") {
    val expected =
      DirectiveDef(
        "foo",
        None,
        List(
          InputValue("arg0", None, ScalarType.StringType, None, Nil),
          InputValue("arg1", None, NullableType(ScalarType.IntType), None, Nil)
        ),
        false,
        List(FIELD)
      )

    val schema =
      Schema("""
        type Query {
          foo: Int
        }

        directive @foo(arg0: String!, arg1: Int) on FIELD
      """)

    assertEquals(schema.map(testDirectiveDefs), List(expected).success)
  }

  test("Directive definition with deprecated argument") {
    val expected =
      DirectiveDef(
        "foo",
        None,
        List(
          InputValue(
            "arg0",
            None,
            ScalarType.StringType,
            None,
            List(Directive("deprecated", List(Binding("reason", Value.StringValue("Use arg1 instead")))))
          ),
          InputValue("arg1", None, ScalarType.StringType, None, Nil)
        ),
        false,
        List(FIELD)
      )

    val schema =
      Schema("""
        type Query {
          foo: Int
        }

        directive @foo(
          arg0: String! @deprecated(reason: "Use arg1 instead"),
          arg1: String!
        ) on FIELD
      """)

    assertEquals(schema.map(testDirectiveDefs), List(expected).success)
  }

  test("Schema with directives") {
    val schema =
    """|schema @foo {
       |  query: Query
       |}
       |scalar Scalar @foo
       |interface Interface @foo {
       |  field(e: Enum, i: Input): Int @foo
       |}
       |type Object implements Interface @foo {
       |  field(e: Enum, i: Input): Int @foo
       |}
       |union Union @foo = Object
       |enum Enum @foo {
       |  VALUE @foo
       |}
       |input Input @foo {
       |  field: Int @foo
       |}
       |directive @foo on SCHEMA|SCALAR|OBJECT|FIELD_DEFINITION|ARGUMENT_DEFINITION|INTERFACE|UNION|ENUM|ENUM_VALUE|INPUT_OBJECT|INPUT_FIELD_DEFINITION
       |""".stripMargin

    val res = schemaParser.parseText(schema)
    val ser = res.map(_.toString)

    assertEquals(ser, schema.success)
  }

  test("Query with directive") {
    val expected =
      Operation(
        UntypedSelect("foo", None, Nil, List(Directive("dir", Nil)),
          UntypedSelect("id", None, Nil, List(Directive("dir", Nil)), Empty)
        ),
        DirectiveMapping.QueryType,
        List(Directive("dir", Nil))
      )

    val query =
      """|query @dir {
         |  foo @dir {
         |    id @dir
         |  }
         |}
         |""".stripMargin

    val res = DirectiveMapping.compiler.compile(query)

    assertEquals(res, expected.success)
  }

  test("Mutation with directive") {
    val expected =
      Operation(
        UntypedSelect("foo", None, Nil, List(Directive("dir", Nil)),
          UntypedSelect("id", None, Nil, List(Directive("dir", Nil)), Empty)
        ),
        DirectiveMapping.MutationType,
        List(Directive("dir", Nil))
      )

    val query =
      """|mutation @dir {
         |  foo @dir {
         |    id @dir
         |  }
         |}
         |""".stripMargin

    val res = DirectiveMapping.compiler.compile(query)

    assertEquals(res, expected.success)
  }

  test("Subscription with directive") {
    val expected =
      Operation(
        UntypedSelect("foo", None, Nil, List(Directive("dir", Nil)),
          UntypedSelect("id", None, Nil, List(Directive("dir", Nil)), Empty)
        ),
        DirectiveMapping.SubscriptionType,
        List(Directive("dir", Nil))
      )

    val query =
      """|subscription @dir {
         |  foo @dir {
         |    id @dir
         |  }
         |}
         |""".stripMargin

    val res = DirectiveMapping.compiler.compile(query)

    assertEquals(res, expected.success)
  }

  test("Fragment with directive") { // TOD: will need new elaborator to expose fragment directives
    val expected =
      Operation(
        UntypedSelect("foo", None, Nil, Nil,
          Narrow(DirectiveMapping.BazType,
            UntypedSelect("baz", None, Nil, List(Directive("dir", Nil)), Empty)
          )
        ),
        DirectiveMapping.QueryType,
        Nil
      )

    val query =
      """|query {
         |  foo {
         |    ... Frag @dir
         |  }
         |}
         |fragment Frag on Baz @dir {
         |  baz @dir
         |}
         |""".stripMargin

    val res = DirectiveMapping.compiler.compile(query)

    assertEquals(res, expected.success)
  }

  test("Inline fragment with directive") { // TOD: will need new elaborator to expose fragment directives
    val expected =
      Operation(
        UntypedSelect("foo", None, Nil, Nil,
          Narrow(DirectiveMapping.BazType,
            UntypedSelect("baz", None, Nil, List(Directive("dir", Nil)), Empty)
          )
        ),
        DirectiveMapping.QueryType,
        Nil
      )

    val query =
      """|query {
         |  foo {
         |    ... on Baz @dir {
         |      baz @dir
         |    }
         |  }
         |}
         |""".stripMargin

    val res = DirectiveMapping.compiler.compile(query)

    assertEquals(res, expected.success)
  }
}

object DirectiveMapping extends TestMapping {
  val schema =
    schema"""
      type Query {
        foo: Bar
      }
      type Mutation {
        foo: Bar
      }
      type Subscription {
        foo: Bar
      }
      interface Bar {
        id: ID
      }
      type Baz implements Bar {
        id: ID
        baz: Int
      }
      directive @dir on QUERY|MUTATION|SUBSCRIPTION|FIELD|FRAGMENT_DEFINITION|FRAGMENT_SPREAD|INLINE_FRAGMENT
    """

  val QueryType = schema.queryType
  val MutationType = schema.mutationType.get
  val SubscriptionType = schema.subscriptionType.get
  val BazType = schema.ref("Baz")

  override val selectElaborator = PreserveArgsElaborator
}
