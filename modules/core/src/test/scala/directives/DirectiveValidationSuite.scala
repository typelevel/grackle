// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package directives

import cats.MonadThrow
import cats.data.Chain
import cats.effect.IO
import cats.implicits._
import munit.CatsEffectSuite

import compiler.PreserveArgsElaborator
import grackle._
import grackle.syntax._
import Query._

final class DirectiveValidationSuite extends CatsEffectSuite {
  test("Schema with validly located directives") {
    val schema =
      Schema(
        """
          schema @onSchema {
            query: Query @onFieldDefinition
          }

          type Query @onObject {
            field: Interface @onFieldDefinition
          }

          interface Interface @onInterface {
            field: String @onFieldDefinition
          }

          type Object1 implements Interface @onObject {
            field: String @onFieldDefinition
            fieldWithArg(arg: Input @onArgumentDefinition): String @onFieldDefinition
            unionField: Union @onFieldDefinition
            enumField: Enum @onFieldDefinition
            scalarField: Scalar @onFieldDefinition
          }

          type Object2 implements Interface @onObject {
            field: String @onFieldDefinition
          }

          union Union @onUnion = Object1 | Object2

          enum Enum @onEnum {
            FOO @onEnumValue
            BAR @onEnumValue
          }

          scalar Scalar @onScalar

          input Input @onInputObject {
            field: String @onInputFieldDefinition
          }

          directive @onSchema on SCHEMA
          directive @onScalar on SCALAR
          directive @onObject on OBJECT
          directive @onFieldDefinition on FIELD_DEFINITION
          directive @onArgumentDefinition on ARGUMENT_DEFINITION
          directive @onInterface on INTERFACE
          directive @onUnion on UNION
          directive @onEnum on ENUM
          directive @onEnumValue on ENUM_VALUE
          directive @onInputObject on INPUT_OBJECT
          directive @onInputFieldDefinition on INPUT_FIELD_DEFINITION
        """
      )

    assertEquals(schema.toProblems, Chain.empty)
  }

  test("Schema with invalidly located directives") {
    val schema =
      Schema(
        """
          schema @onFieldDefinition {
            query: Query @onSchema
          }

          type Query @onSchema {
            field: Interface @onSchema
          }

          interface Interface @onSchema {
            field: String @onSchema
          }

          type Object1 implements Interface @onSchema {
            field: String @onSchema
            fieldWithArg(arg: Input @onSchema): String @onSchema
            unionField: Union @onSchema
            enumField: Enum @onSchema
            scalarField: Scalar @onSchema
          }

          type Object2 implements Interface @onSchema {
            field: String @onSchema
          }

          union Union @onSchema = Object1 | Object2

          enum Enum @onSchema {
            FOO @onSchema
            BAR @onSchema
          }

          scalar Scalar @onSchema

          input Input @onSchema {
            field: String @onSchema
          }

          directive @onSchema on SCHEMA
          directive @onFieldDefinition on FIELD_DEFINITION
        """
      )

    val problems =
      Chain(
        Problem("Directive 'onFieldDefinition' is not allowed on SCHEMA"),
        Problem("Directive 'onSchema' is not allowed on FIELD_DEFINITION"),
        Problem("Directive 'onSchema' is not allowed on OBJECT"),
        Problem("Directive 'onSchema' is not allowed on FIELD_DEFINITION"),
        Problem("Directive 'onSchema' is not allowed on INTERFACE"),
        Problem("Directive 'onSchema' is not allowed on FIELD_DEFINITION"),
        Problem("Directive 'onSchema' is not allowed on OBJECT"),
        Problem("Directive 'onSchema' is not allowed on FIELD_DEFINITION"),
        Problem("Directive 'onSchema' is not allowed on FIELD_DEFINITION"),
        Problem("Directive 'onSchema' is not allowed on ARGUMENT_DEFINITION"),
        Problem("Directive 'onSchema' is not allowed on FIELD_DEFINITION"),
        Problem("Directive 'onSchema' is not allowed on FIELD_DEFINITION"),
        Problem("Directive 'onSchema' is not allowed on FIELD_DEFINITION"),
        Problem("Directive 'onSchema' is not allowed on OBJECT"),
        Problem("Directive 'onSchema' is not allowed on FIELD_DEFINITION"),
        Problem("Directive 'onSchema' is not allowed on UNION"),
        Problem("Directive 'onSchema' is not allowed on ENUM"),
        Problem("Directive 'onSchema' is not allowed on ENUM_VALUE"),
        Problem("Directive 'onSchema' is not allowed on ENUM_VALUE"),
        Problem("Directive 'onSchema' is not allowed on SCALAR"),
        Problem("Directive 'onSchema' is not allowed on INPUT_OBJECT"),
        Problem("Directive 'onSchema' is not allowed on INPUT_FIELD_DEFINITION")
      )

    assertEquals(schema.toProblems, problems)
  }

  test("Schema with invalid directive arguments") {
    val schema =
      Schema(
        """
          type Query {
            f1: Int @withArg(i: 1)
            f2: Int @withArg
            f3: Int @withArg(i: "foo")
            f4: Int @withArg(x: "foo")
            f5: Int @withRequiredArg(i: 1)
            f6: Int @withRequiredArg
            f7: Int @withRequiredArg(i: "foo")
            f8: Int @withRequiredArg(x: "foo")
            f9: Int @deprecated(reason: "foo")
            f10: Int @deprecated
            f11: Int @deprecated(reason: 1)
            f12: Int @deprecated(x: "foo")
          }

          directive @withArg(i: Int) on FIELD_DEFINITION
          directive @withRequiredArg(i: Int!) on FIELD_DEFINITION
        """
      )

    val problems =
      Chain(
        Problem("""Expected Int found '"foo"' for 'i' in directive withArg"""),
        Problem("""Unknown argument(s) 'x' in directive withArg"""),
        Problem("""Value of type Int required for 'i' in directive withRequiredArg"""),
        Problem("""Expected Int found '"foo"' for 'i' in directive withRequiredArg"""),
        Problem("""Unknown argument(s) 'x' in directive withRequiredArg"""),
        Problem("""Expected String found '1' for 'reason' in directive deprecated"""),
        Problem("""Unknown argument(s) 'x' in directive deprecated"""),
      )

    assertEquals(schema.toProblems, problems)
  }

  test("Query with validly located directives") {
    val expected =
      List(
        Operation(
          UntypedSelect("foo", None, Nil, List(Directive("onField", Nil)),
            Group(
              List(
                UntypedSelect("bar",None, Nil, List(Directive("onField", Nil)), Empty),
                UntypedSelect("bar",None, Nil, List(Directive("onField", Nil)), Empty)
              )
            )
          ),
          ExecutableDirectiveMapping.QueryType,
          List(Directive("onQuery",List()))
        ),
        Operation(
          UntypedSelect("foo",None, Nil, List(Directive("onField", Nil)), Empty),
          ExecutableDirectiveMapping.MutationType,
          List(Directive("onMutation",List()))
        ),
        Operation(
          UntypedSelect("foo",None, Nil, List(Directive("onField", Nil)), Empty),
          ExecutableDirectiveMapping.SubscriptionType,
          List(Directive("onSubscription",List()))
        )
      )

    val query =
      """|query ($var: Boolean @onVariableDefinition) @onQuery {
         |  foo @onField {
         |    ...Frag @onFragmentSpread
         |    ... @onInlineFragment {
         |      bar @onField
         |    }
         |  }
         |}
         |
         |mutation @onMutation {
         |  foo @onField
         |}
         |
         |subscription @onSubscription {
         |  foo @onField
         |}
         |
         |fragment Frag on Foo @onFragmentDefinition {
         |  bar @onField
         |}
         |""".stripMargin

    val res = ExecutableDirectiveMapping.compileAllOperations(query)
    //println(res)

    assertEquals(res, expected.success)
  }

  test("Query with invalidly located directives") {
    val problems =
      Chain(
        Problem("Directive 'onField' is not allowed on VARIABLE_DEFINITION"),
        Problem("Directive 'onField' is not allowed on QUERY"),
        Problem("Directive 'onQuery' is not allowed on FIELD"),
        Problem("Directive 'onField' is not allowed on FRAGMENT_SPREAD"),
        Problem("Directive 'onField' is not allowed on INLINE_FRAGMENT"),
        Problem("Directive 'onQuery' is not allowed on FIELD"),
        Problem("Directive 'onField' is not allowed on FRAGMENT_DEFINITION"),
        Problem("Directive 'onQuery' is not allowed on FIELD"),
        Problem("Directive 'onField' is not allowed on MUTATION"),
        Problem("Directive 'onQuery' is not allowed on FIELD"),
        Problem("Directive 'onField' is not allowed on FRAGMENT_DEFINITION"),
        Problem("Directive 'onQuery' is not allowed on FIELD"),
        Problem("Directive 'onField' is not allowed on SUBSCRIPTION"),
        Problem("Directive 'onQuery' is not allowed on FIELD"),
        Problem("Directive 'onField' is not allowed on FRAGMENT_DEFINITION"),
        Problem("Directive 'onQuery' is not allowed on FIELD")
      )

    val query =
      """|query ($var: Boolean @onField) @onField {
         |  foo @onQuery {
         |    ...Frag @onField
         |    ... @onField {
         |      bar @onQuery
         |    }
         |  }
         |}
         |
         |mutation @onField {
         |  foo @onQuery
         |}
         |
         |subscription @onField {
         |  foo @onQuery
         |}
         |
         |fragment Frag on Foo @onField {
         |  bar @onQuery
         |}
         |""".stripMargin

    val res = ExecutableDirectiveMapping.compileAllOperations(query)
    //println(res)

    assertEquals(res.toProblems, problems)
  }

  test("Query with invalid directive arguments") {
    val problems =
      Chain(
        Problem("""Expected Int found '"foo"' for 'i' in directive withArg"""),
        Problem("""Unknown argument(s) 'x' in directive withArg"""),
        Problem("""Value of type Int required for 'i' in directive withRequiredArg"""),
        Problem("""Expected Int found '"foo"' for 'i' in directive withRequiredArg"""),
        Problem("""Unknown argument(s) 'x' in directive withRequiredArg""")
      )

    val query =
      """|query {
         |  foo {
         |    b1: bar @withArg(i: 1)
         |    b2: bar @withArg
         |    b3: bar @withArg(i: "foo")
         |    b4: bar @withArg(x: "foo")
         |    b5: bar @withRequiredArg(i: 1)
         |    b6: bar @withRequiredArg
         |    b7: bar @withRequiredArg(i: "foo")
         |    b8: bar @withRequiredArg(x: "foo")
         |  }
         |}
         |""".stripMargin

    val res = ExecutableDirectiveMapping.compileAllOperations(query)
    //println(res)

    assertEquals(res.toProblems, problems)
  }
}

object ExecutableDirectiveMapping extends Mapping[IO] {
  val M: MonadThrow[IO] = MonadThrow[IO]

  val schema =
    schema"""
      type Query {
        foo: Foo
      }
      type Mutation {
        foo: String
      }
      type Subscription {
        foo: String
      }
      type Foo {
        bar: String
      }
      directive @onQuery on QUERY
      directive @onMutation on MUTATION
      directive @onSubscription on SUBSCRIPTION
      directive @onField on FIELD
      directive @onFragmentDefinition on FRAGMENT_DEFINITION
      directive @onFragmentSpread on FRAGMENT_SPREAD
      directive @onInlineFragment on INLINE_FRAGMENT
      directive @onVariableDefinition on VARIABLE_DEFINITION

      directive @withArg(i: Int) on FIELD
      directive @withRequiredArg(i: Int!) on FIELD
    """

  val QueryType = schema.queryType
  val MutationType = schema.mutationType.get
  val SubscriptionType = schema.subscriptionType.get

  val typeMappings: List[TypeMapping] = Nil

  override val selectElaborator = PreserveArgsElaborator

  def compileAllOperations(text: String): Result[List[Operation]] =
    QueryParser.parseText(text).flatMap {
      case (ops, frags) => ops.parTraverse(compiler.compileOperation(_, None, frags))
    }
}
