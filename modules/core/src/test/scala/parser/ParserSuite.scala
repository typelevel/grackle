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

package parser

import cats.data.NonEmptyChain
import munit.CatsEffectSuite

import grackle.{Ast, GraphQLParser, Problem, Result}
import grackle.syntax._
import Ast._, OperationType._, OperationDefinition._, Selection._, Value._, Type.Named

final class ParserSuite extends CatsEffectSuite {
  test("simple query") {
    val query = doc"""
      query {
        character(id: 1000) {
          name
        }
      }
    """

    val expected =
      Operation(Query, None, Nil, Nil,
        List(
          Field(None, Name("character"), List((Name("id"), IntValue(1000))), Nil,
            List(
              Field(None, Name("name"), Nil, Nil, Nil)
            )
          )
        )
      )

    assertEquals(query, List(expected))
  }

  test("multiple parameters (commas)") {
    val query = """
      query {
        wibble(foo: "a", bar: "b", baz: 3) {
          quux
        }
      }
    """

    val expected =
      Operation(
        Query,None,Nil,Nil,
        List(
          Field(None,Name("wibble"),
            List(
              (Name("foo"),StringValue("a")),
              (Name("bar"),StringValue("b")),
              (Name("baz"),IntValue(3))
            ), Nil,
            List(
              Field(None,Name("quux"),Nil,Nil,Nil)
            )
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("multiple parameters (no commas)") {
    val query = """
      query {
        wibble(foo: "a" bar: "b" baz: 3) {
          quux
        }
      }
    """

    val expected =
      Operation(
        Query,None,Nil,Nil,
        List(
          Field(None,Name("wibble"),
            List(
              (Name("foo"),StringValue("a")),
              (Name("bar"),StringValue("b")),
              (Name("baz"),IntValue(3))
            ), Nil,
            List(
              Field(None,Name("quux"),Nil,Nil,Nil)
            )
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("introspection query") {
    val query = """
      query IntrospectionQuery {
        __schema {
          queryType {
            name
          }
          mutationType {
            name
          }
          subscriptionType {
            name
          }
        }
      }
    """

    val expected =
      Operation(Query,Some(Name("IntrospectionQuery")),Nil,Nil,
        List(
          Field(None,Name("__schema"),Nil,Nil,
            List(
              Field(None,Name("queryType"),Nil,Nil,
                List(
                  Field(None,Name("name"),Nil,Nil,Nil)
                )
              ),
              Field(None,Name("mutationType"),Nil,Nil,
                List(
                  Field(None,Name("name"),Nil,Nil,Nil)
                )
              ),
              Field(None,Name("subscriptionType"),Nil,Nil,
                List(
                  Field(None,Name("name"),Nil,Nil,Nil)
                )
              )
            )
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("shorthand query") {
    val query = """
      {
        hero(episode: NEWHOPE) {
          name
          friends {
            name
            friends {
              name
            }
          }
        }
      }
    """

    val expected =
      QueryShorthand(
        List(
          Field(None,Name("hero"),List((Name("episode"),EnumValue(Name("NEWHOPE")))),Nil,
            List(
              Field(None,Name("name"),Nil,Nil,Nil),
              Field(None,Name("friends"),Nil,Nil,
                List(
                  Field(None,Name("name"),Nil,Nil,Nil),
                  Field(None,Name("friends"),Nil,Nil,
                    List(
                      Field(None,Name("name"),Nil,Nil,Nil)
                    )
                  )
                )
              )
            )
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("field alias") {
    val query = """
      {
        user(id: 4) {
          id
          name
          smallPic: profilePic(size: 64)
          bigPic: profilePic(size: 1024)
        }
      }
    """

    val expected =
      QueryShorthand(
        List(
          Field(None, Name("user"), List((Name("id"), IntValue(4))), Nil,
            List(
              Field(None, Name("id"), Nil, Nil, Nil),
              Field(None, Name("name"), Nil, Nil, Nil),
              Field(Some(Name("smallPic")), Name("profilePic"), List((Name("size"), IntValue(64))), Nil, Nil),
              Field(Some(Name("bigPic")), Name("profilePic"), List((Name("size"), IntValue(1024))), Nil, Nil)
            )
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("multiple root fields") {
    val query = """
      {
        luke: character(id: "1000") {
          name
        }
        darth: character(id: "1001") {
          name
        }
      }
    """

    val expected =
      QueryShorthand(
        List(
          Field(Some(Name("luke")), Name("character"), List((Name("id"), StringValue("1000"))), Nil,
            List(
              Field(None, Name("name"), Nil, Nil, Nil))),
          Field(Some(Name("darth")), Name("character"), List((Name("id"), StringValue("1001"))), Nil,
            List(
              Field(None, Name("name"), Nil, Nil, Nil)
            )
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("variables") {
    val query = """
      query getZuckProfile($devicePicSize: Int) {
        user(id: 4) {
          id
          name
          profilePic(size: $devicePicSize)
        }
      }
    """

    val expected =
      Operation(Query, Some(Name("getZuckProfile")),
        List(VariableDefinition(Name("devicePicSize"), Named(Name("Int")), None, Nil)),
        Nil,
        List(
          Field(None, Name("user"), List((Name("id"), IntValue(4))), Nil,
            List(
              Field(None, Name("id"), Nil, Nil, Nil),
              Field(None, Name("name"), Nil, Nil, Nil),
              Field(None, Name("profilePic"), List((Name("size"), Variable(Name("devicePicSize")))), Nil, Nil)
            )
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("variables with default value") {
    val query = """
      query getZuckProfile($devicePicSize: Int = 10) {
        user(id: 4) {
          id
          name
          profilePic(size: $devicePicSize)
        }
      }
    """

    val expected =
      Operation(Query, Some(Name("getZuckProfile")),
        List(VariableDefinition(Name("devicePicSize"), Named(Name("Int")), Some(IntValue(10)), Nil)),
        Nil,
        List(
          Field(None, Name("user"), List((Name("id"), IntValue(4))), Nil,
            List(
              Field(None, Name("id"), Nil, Nil, Nil),
              Field(None, Name("name"), Nil, Nil, Nil),
              Field(None, Name("profilePic"), List((Name("size"), Variable(Name("devicePicSize")))), Nil, Nil)
            )
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("variables with directive") {
    val query = """
      query getZuckProfile($devicePicSize: Int @dir) {
        user(id: 4) {
          id
          name
          profilePic(size: $devicePicSize)
        }
      }
    """

    val expected =
      Operation(Query, Some(Name("getZuckProfile")),
        List(VariableDefinition(Name("devicePicSize"), Named(Name("Int")), None, List(Directive(Name("dir"), Nil)))),
        Nil,
        List(
          Field(None, Name("user"), List((Name("id"), IntValue(4))), Nil,
            List(
              Field(None, Name("id"), Nil, Nil, Nil),
              Field(None, Name("name"), Nil, Nil, Nil),
              Field(None, Name("profilePic"), List((Name("size"), Variable(Name("devicePicSize")))), Nil, Nil)
            )
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("comments") {
    val query = """
      #comment at start of document
      query IntrospectionQuery { #comment at end of line
        __schema {
          queryType {
            name#comment eol no space
          }
          mutationType {
            name
            #several comments
            #one after another
          }
          subscriptionType {
            name
          }
        }
      }
      #comment at end of document
    """

    val expected =
      Operation(Query,Some(Name("IntrospectionQuery")),Nil,Nil,
        List(
          Field(None,Name("__schema"),Nil,Nil,
            List(
              Field(None,Name("queryType"),Nil,Nil,
                List(
                  Field(None,Name("name"),Nil,Nil,Nil)
                )
              ),
              Field(None,Name("mutationType"),Nil,Nil,
                List(
                  Field(None,Name("name"),Nil,Nil,Nil)
                )
              ),
              Field(None,Name("subscriptionType"),Nil,Nil,
                List(
                  Field(None,Name("name"),Nil,Nil,Nil)
                )
              )
            )
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("invalid document") {
    GraphQLParser.Document.parseAll("scalar Foo woozle").toOption match {
      case Some(_) => fail("should have failed")
      case None    => ()
    }
  }

  test("fragment") {
    val query = """
      query {
        character(id: 1000) {
          ...frag
          ... on Character {
            age
          }
        }
      }

      fragment frag on Character {
        name
      }
    """

    val expected =
      List(
        Operation(Query, None, Nil, Nil,
          List(
            Field(None, Name("character"), List((Name("id"), IntValue(1000))), Nil,
              List(
                FragmentSpread(Name("frag"),Nil),
                InlineFragment(
                  Some(Named(Name("Character"))),
                  Nil,
                  List(
                    Field(None,Name("age"),Nil ,Nil ,Nil)
                  )
                )
              )
            )
          )
        ),
        FragmentDefinition(
          Name("frag"),
          Named(Name("Character")),
          Nil,
          List(
            Field(None,Name("name"),Nil ,Nil ,Nil)
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(xs) => assertEquals(xs, expected)
      case _ => assert(false)
    }

  }

  test("fragment with standard directive") {
    val query = """
      query frag($expanded: Boolean){
        character(id: 1000) {
          name
          ... @include(if: $expanded) {
            age
          }
        }
      }
    """

    val expected =
      Operation(
        Query,
        Some(Name("frag")),
        List(VariableDefinition(Name("expanded"),Named(Name("Boolean")),None, Nil)),
        Nil,
        List(
          Field(None, Name("character"), List((Name("id"), IntValue(1000))), Nil,
            List(
              Field(None, Name("name"), Nil, Nil, Nil),
              InlineFragment(
                None,
                List(Directive(Name("include"),List((Name("if"),Variable(Name("expanded")))))),
                List(Field(None,Name("age"),List(),List(),List()))
              )
            )
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("fragment with custorm directive") {
    val query = """
      query {
        character(id: 1000) {
          name
          ... @dir {
            age
          }
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
          Field(None, Name("character"), List((Name("id"), IntValue(1000))), Nil,
            List(
              Field(None, Name("name"), Nil, Nil, Nil),
              InlineFragment(
                None,
                List(Directive(Name("dir"), Nil)),
                List(Field(None,Name("age"),List(),List(),List()))
              )
            )
          )
        )
      )

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("value literals") {

    def assertParse(input: String, expected: Value) =
      GraphQLParser.Value.parseAll(input).toOption match {
        case Some(v) => assertEquals(v, expected)
        case _ => assert(false)
      }

    assertParse("\"fooλ\"", StringValue("fooλ"))
    assertParse("\"\\u03BB\"", StringValue("λ"))
    assertParse("\"\\\" \\\\ \\/ \\b \\f \\n \\r \\t\"", StringValue("\" \\ / \b \f \n \r \t"))

    assertParse("123.2", FloatValue(123.2d))
    assertParse("123E2", FloatValue(123E2d))
    assertParse("123.2E2", FloatValue(123.2E2d))

    assertParse("123", IntValue(123))
    assertParse("-123", IntValue(-123))

    assertParse("true", BooleanValue(true))
    assertParse("false", BooleanValue(false))

    assertParse("null", NullValue)

    assertParse("Foo", EnumValue(Name("Foo")))

    assertParse("[1, \"foo\"]", ListValue(List(IntValue(1), StringValue("foo"))))

    assertParse("{foo: 1, bar: \"baz\"}", ObjectValue(List(Name("foo") -> IntValue(1), Name("bar") -> StringValue("baz"))))

    assertParse("\"\"\"one\"\"\"", StringValue("one"))
    assertParse("\"\"\"    \n\n   first\n   \tλ\n  123\n\n\n   \t\n\n\"\"\"", StringValue(" first\n \tλ\n123"))
  }

  test("parse object type extension") {
    val schema = """
      extend type Foo {
        bar: Int
      }
    """

    val expected =
      List(
        ObjectTypeExtension(Named(Name("Foo")), List(FieldDefinition(Name("bar"),None,Nil,Named(Name("Int")),Nil)), Nil, Nil)
      )

    val res = GraphQLParser.Document.parseAll(schema).toOption
    assert(res == Some(expected))
  }

  test("parse schema extension") {
    val schema = """
      extend schema {
        query: Query
      }
    """

    val expected =
      List(
        SchemaExtension(List(RootOperationTypeDefinition(OperationType.Query, Named(Name("Query")), Nil)), Nil)
      )

    val res = GraphQLParser.Document.parseAll(schema).toOption
    assert(res == Some(expected))
  }

  test("keywords parsed non-greedily (1)") {
    val schema =
      """|extend type Name {
         |  value:String
         |}""".stripMargin

    val expected =
      List(
        ObjectTypeExtension(
          Named(Name("Name")),
          List(
            FieldDefinition(Name("value"), None, Nil, Named(Name("String")), Nil)
          ),
          Nil,
          Nil
        )
      )

    val res = GraphQLParser.Document.parseAll(schema).toOption
    assertEquals(res, Some(expected))
  }

  test("keywords parsed non-greedily (2)") {
    val schema =
      """|extendtypeName {
         |  value:String
         |}""".stripMargin

    val expected =
      NonEmptyChain(
        Problem(
          """|Parse error at line 0 column 6
            |extendtypeName {
            |      ^""".stripMargin
        )
      )

    val res = GraphQLParser.toResult(schema, GraphQLParser.Document.parseAll(schema))
    assertEquals(res, Result.Failure(expected))
  }
}
