// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package parser

import atto.Atto._
import cats.tests.CatsSuite

import edu.gemini.grackle.{ Ast, Parser }
import Ast._, OperationType._, OperationDefinition._, Selection._, Value._

final class ParserSuite extends CatsSuite {
  val text = """
    query {
      character(id: 1000) {
        name
      }
    }
  """

  test("simple query") {
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

    Parser.Document.parseOnly(text).option match {
      case Some(List(Left(q))) => assert(q == expected)
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

    Parser.Document.parseOnly(text).option match {
      case Some(List(Left(q))) => assert(q == expected)
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

    Parser.Document.parseOnly(text).option match {
      case Some(List(Left(q))) => assert(q == expected)
    }
  }
}
