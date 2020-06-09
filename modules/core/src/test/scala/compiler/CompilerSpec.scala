// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.data.Ior
import cats.implicits._
import cats.tests.CatsSuite

import edu.gemini.grackle._
import Query._, Predicate._, Value._
import QueryCompiler._, ComponentElaborator.{ Mapping, TrivialJoin }

final class CompilerSuite extends CatsSuite {
  test("simple query") {
    val text = """
      query {
        character(id: "1000") {
          name
        }
      }
    """

    val expected =
      Select("character", List(Binding("id", StringValue("1000"))),
        Select("name", Nil)
      )

    val res = QueryParser.parseText(text)
    assert(res == Ior.Right((expected, Nil)))
  }

  test("simple nested query") {
    val text = """
      query {
        character(id: "1000") {
          name
          friends {
            name
          }
        }
      }
    """

    val expected =
      Select(
        "character", List(Binding("id", StringValue("1000"))),
        Select("name", Nil) ~
          Select(
            "friends", Nil,
            Select("name", Nil)
          )
      )

    val res = QueryParser.parseText(text)
    assert(res == Ior.Right((expected, Nil)))
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
      Select(
        "hero", List(Binding("episode", UntypedEnumValue("NEWHOPE"))),
        Select("name", Nil, Empty) ~
        Select("friends", Nil,
          Select("name", Nil, Empty) ~
          Select("friends", Nil,
            Select("name", Nil, Empty)
          )
        )
      )

    val res = QueryParser.parseText(text)
    assert(res == Ior.Right((expected, Nil)))
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
      Select("user", List(Binding("id", IntValue(4))),
        Group(List(
          Select("id", Nil, Empty),
          Select("name", Nil, Empty),
          Rename("smallPic", Select("profilePic", List(Binding("size", IntValue(64))), Empty)),
          Rename("bigPic", Select("profilePic", List(Binding("size", IntValue(1024))), Empty))
        ))
      )

    val res = QueryParser.parseText(text)
    assert(res == Ior.Right((expected, Nil)))
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
      Select(
        "__schema", Nil,
        Select("queryType", Nil, Select("name", Nil, Empty)) ~
        Select("mutationType", Nil, Select("name", Nil, Empty)) ~
        Select("subscriptionType", Nil, Select("name", Nil, Empty))
      )

    val res = QueryParser.parseText(text)
    assert(res == Ior.Right((expected, Nil)))
  }

  test("simple selector elaborated query") {
    val text = """
      query {
        character(id: "1000") {
          name
          friends {
            name
          }
        }
      }
    """

    val expected =
      Unique(Eql(FieldPath(List("id")), Const("1000")),
        Select("name", Nil) ~
          Select(
            "friends", Nil,
            Select("name", Nil)
          )
      )

    val res = SimpleCompiler.compile(text)

    assert(res == Ior.Right(expected))
  }

  test("simple component elaborated query") {
    val text = """
      query {
        componenta {
          fielda1
          fielda2 {
            componentb {
              fieldb1
              fieldb2 {
                componentc
              }
            }
          }
        }
      }
    """

    val expected =
      Wrap("componenta",
        Component("ComponentA", TrivialJoin,
          Select("componenta", Nil,
            Select("fielda1", Nil) ~
            Select("fielda2", Nil,
              Wrap("componentb",
                Component("ComponentB", TrivialJoin,
                  Select("componentb", Nil,
                    Select("fieldb1", Nil) ~
                    Select("fieldb2", Nil,
                      Wrap("componentc",
                        Component("ComponentC", TrivialJoin,
                          Select("componentc", Nil, Empty)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

    val res = ComposedCompiler.compile(text)

    assert(res == Ior.Right(expected))
  }
}

object SimpleData {
  val schema =
    Schema(
      """
        type Query {
          character(id: String!): Character
        }
        type Character {
          id: String!
          name: String
          friends: [Character!]
        }
      """
    ).right.get
}

object SimpleCompiler extends QueryCompiler(SimpleData.schema) {
  val selectElaborator = new SelectElaborator(Map(
    SimpleData.schema.ref("Query") -> {
      case Select("character", List(Binding("id", StringValue(id))), child) =>
        Unique(Eql(FieldPath(List("id")), Const(id)), child).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

object ComposedData {
  val schema =
    Schema(
      """
        type Query {
          componenta: ComponentA!
        }
        type ComponentA {
          fielda1: String!
          fielda2: FieldA2
        }
        type FieldA2 {
          componentb: ComponentB
        }
        type ComponentB {
          fieldb1: String!
          fieldb2: FieldB2
        }
        type FieldB2 {
          componentc: ComponentC
        }
        type ComponentC {
        }
      """
    ).right.get
}

object ComposedCompiler extends QueryCompiler(ComposedData.schema) {
  val componentElaborator = ComponentElaborator(
    Mapping(ComposedData.schema.ref("Query"), "componenta", "ComponentA"),
    Mapping(ComposedData.schema.ref("FieldA2"), "componentb", "ComponentB"),
    Mapping(ComposedData.schema.ref("FieldB2"), "componentc", "ComponentC")
  )

  val phases = List(componentElaborator)
}
