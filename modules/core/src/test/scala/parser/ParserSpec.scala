// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package parser

import atto.Atto._
import cats.tests.CatsSuite

import edu.gemini.grackle.{ Ast, GraphQLParser }
import Ast._, OperationType._, OperationDefinition._, Selection._, Value._, Type.Named

final class ParserSuite extends CatsSuite {
  test("simple query") {
    val text = """
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

    GraphQLParser.Document.parseOnly(text).option match {
      case Some(List(q)) => assert(q == expected)
    }
  }

  test("introspection query") {
    val text = """
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

    GraphQLParser.Document.parseOnly(text).option match {
      case Some(List(q)) => assert(q == expected)
    }
  }

  test("shorthand query") {
    val text = """
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

    GraphQLParser.Document.parseOnly(text).option match {
      case Some(List(q)) => assert(q == expected)
    }
  }

  test("field alias") {
    val text = """
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

    GraphQLParser.Document.parseOnly(text).option match {
      case Some(List(q)) => assert(q == expected)
    }
  }

  test("multiple root fields") {
    val text = """
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

    GraphQLParser.Document.parseOnly(text).option match {
      case Some(List(q)) => assert(q == expected)
    }
  }

  test("variables") {
    val text = """
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

    GraphQLParser.Document.parseOnly(text).option match {
      case Some(List(q)) => assert(q == expected)
    }
  }
}
