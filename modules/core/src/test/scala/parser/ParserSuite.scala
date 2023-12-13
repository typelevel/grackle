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

import munit.CatsEffectSuite

import grackle.{Ast, GraphQLParser, Result}
import grackle.syntax._
import Ast._, OperationType._, OperationDefinition._, Selection._, Value._, Type.Named

final class ParserSuite extends CatsEffectSuite {
  val parser = mkParser()

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

    parser.parseText(query).toOption match {
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

    parser.parseText(query).toOption match {
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

    parser.parseText(query).toOption match {
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

    parser.parseText(query).toOption match {
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

    parser.parseText(query).toOption match {
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

    parser.parseText(query).toOption match {
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

    parser.parseText(query).toOption match {
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

    parser.parseText(query).toOption match {
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

    parser.parseText(query).toOption match {
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

    parser.parseText(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("invalid document") {
    parser.parseText("scalar Foo woozle").toOption match {
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

    parser.parseText(query).toOption match {
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

    parser.parseText(query).toOption match {
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

    parser.parseText(query).toOption match {
      case Some(List(q)) => assertEquals(q, expected)
      case _ => assert(false)
    }
  }

  test("value literals") {

    def assertParse(input: String, expected: Value) =
      parser.parseText(s"query { foo(bar: $input) }").toOption match {
        case Some(List(Operation(_, _, _, _,List(Field(_, _, List((_, v)), _, _))))) =>
          assertEquals(v, expected)
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

  test("outsized int") {
    val query =
      """|query {
         |  foo {
         |    bar {
         |      baz(id: 2147483648)
         |    }
         |  }
         |}""".stripMargin

    val expected =
      """|...
         |  foo {
         |    bar {
         |      baz(id: 2147483648)
         |                        ^
         |expectation:
         |* must fail: 2147483648 is larger than max int
         |    }
         |  }""".stripMargin

    val res = parser.parseText(query)

    assertEquals(res, Result.failure(expected))
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

    val res = parser.parseText(schema).toOption
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

    val res = parser.parseText(schema).toOption
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

    val res = parser.parseText(schema).toOption
    assertEquals(res, Some(expected))
  }

  test("keywords parsed non-greedily (2)") {
    val schema =
      """|extendtypeName {
         |  value: String
         |}""".stripMargin

    val expected =
      """|extendtypeName {
         |      ^
         |expectation:
         |* must fail but matched with t
         |  value: String
         |}""".stripMargin

    val res = parser.parseText(schema)
    assertEquals(res, Result.failure(expected))
  }

  test("deep query") {
    def mkQuery(depth: Int): String = {
      val depth0 = depth - 1
      "query{" + ("f{" *depth0) + "f" + ("}" * depth0) + "}"
    }

    val limit = 5
    val limitedParser = mkParser(maxSelectionDepth = limit)

    val queryOk = mkQuery(limit)
    val queryFail = mkQuery(limit + 1)

    val expectedFail =
      """|query{f{f{f{f{f{f}}}}}}
         |                ^
         |expectation:
         |* must fail: exceeded maximum selection depth""".stripMargin

    val resOk = limitedParser.parseText(queryOk)
    assert(resOk.hasValue)

    val resFail = limitedParser.parseText(queryFail)
    assertEquals(resFail, Result.failure(expectedFail))
  }

  test("wide query") {
    def mkQuery(width: Int): String =
      "query{r{" + ("f," * (width - 1) + "f") + "}}"

    val limit = 5
    val limitedParser = mkParser(maxSelectionWidth = limit)

    val queryOk = mkQuery(limit)
    val queryFail = mkQuery(limit + 1)

    val expectedFail =
      """|query{r{f,f,f,f,f,f}}
         |                  ^
         |expectation:
         |* must be char: '}'""".stripMargin

    val resOk = limitedParser.parseText(queryOk)
    assert(resOk.hasValue)

    val resFail = limitedParser.parseText(queryFail)
    assertEquals(resFail, Result.failure(expectedFail))
  }

  test("deep list value") {
    def mkQuery(depth: Int): String =
      "query{f(l: " + ("[" *depth) + "0" + ("]" * depth) + "){f}}"

    val limit = 5
    val limitedParser = mkParser(maxInputValueDepth = limit)

    val queryOk = mkQuery(limit)
    val queryFail = mkQuery(limit + 1)

    val expectedFail =
      """|query{f(l: [[[[[[0]]]]]]){f}}
         |                 ^
         |expectation:
         |* must fail: exceeded maximum input value depth""".stripMargin

    val resOk = limitedParser.parseText(queryOk)
    assert(resOk.hasValue)

    val resFail = limitedParser.parseText(queryFail)
    assertEquals(resFail, Result.failure(expectedFail))
  }

  test("deep input object value") {
    def mkQuery(depth: Int): String =
      "query{f(l: " + ("{m:" *depth) + "0" + ("}" * depth) + "){f}}"

    val limit = 5
    val limitedParser = mkParser(maxInputValueDepth = limit)

    val queryOk = mkQuery(limit)
    val queryFail = mkQuery(limit + 1)

    val expectedFail =
      """|query{f(l: {m:{m:{m:{m:{m:{m:0}}}}}}){f}}
         |                           ^
         |expectation:
         |* must fail: exceeded maximum input value depth""".stripMargin

    val resOk = limitedParser.parseText(queryOk)
    assert(resOk.hasValue)

    val resFail = limitedParser.parseText(queryFail)
    assertEquals(resFail, Result.failure(expectedFail))
  }

  test("deep variable type") {
    def mkQuery(depth: Int): String =
      "query($l: " + ("[" *depth) + "Int" + ("]" * depth) + "){f(a:$l)}"

    val limit = 5
    val limitedParser = mkParser(maxListTypeDepth = limit)

    val queryOk = mkQuery(limit)
    val queryFail = mkQuery(limit + 1)

    val expectedFail =
      """|query($l: [[[[[[Int]]]]]]){f(a:$l)}
         |                ^
         |expectation:
         |* must fail: exceeded maximum list type depth""".stripMargin

    val resOk = limitedParser.parseText(queryOk)
    assert(resOk.hasValue)

    val resFail = limitedParser.parseText(queryFail)
    assertEquals(resFail, Result.failure(expectedFail))
  }

  def mkParser(
    maxSelectionDepth: Int = GraphQLParser.defaultConfig.maxSelectionDepth,
    maxSelectionWidth: Int = GraphQLParser.defaultConfig.maxSelectionWidth,
    maxInputValueDepth: Int = GraphQLParser.defaultConfig.maxInputValueDepth,
    maxListTypeDepth: Int = GraphQLParser.defaultConfig.maxListTypeDepth,
  ): GraphQLParser =
    GraphQLParser(
      GraphQLParser.Config(
        maxSelectionDepth = maxSelectionDepth,
        maxSelectionWidth = maxSelectionWidth,
        maxInputValueDepth = maxInputValueDepth,
        maxListTypeDepth = maxListTypeDepth,
        terseError = false
      )
    )
}
