// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package parser

import cats.tests.CatsSuite

import edu.gemini.grackle.{ Ast, GraphQLParser }
import Ast._, OperationType._, OperationDefinition._, Selection._, Value._, Type.Named

final class ParserSuite extends CatsSuite {
  test("simple query") {
    val query = """
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

    GraphQLParser.Document.parseAll(query).toOption match {
      case Some(List(q)) => assert(q == expected)
      case _ => assert(false)
    }
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
      case Some(List(q)) => assert(q == expected)
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
      case Some(List(q)) => assert(q == expected)
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
      case Some(List(q)) => assert(q == expected)
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
      case Some(List(q)) => assert(q == expected)
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
      case Some(List(q)) => assert(q == expected)
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
      case Some(List(q)) => assert(q == expected)
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
        List(VariableDefinition(Name("devicePicSize"), Named(Name("Int")), None)),
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
      case Some(List(q)) => assert(q == expected)
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
      case Some(List(q)) => assert(q == expected)
      case _ => assert(false)
    }
  }

  test("invalid document") {
    GraphQLParser.Document.parseAll("scalar Foo woozle").toOption match {
      case Some(_) => fail("should have failed")
      case None    => succeed
    }
  }

  test("value literals") {

    def assertParse(input: String, expected: Value) = {
      val r = GraphQLParser.Value.parseAll(input)
      println(r)
      GraphQLParser.Value.parseAll(input).toOption match {
        case Some(v) => assert(v == expected)
        case _ => assert(false)
      }
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
    assertParse("\"\"\"first\n    second\n  third\n\"\"\"", StringValue("first\n  second\nthird"))
  }

}
