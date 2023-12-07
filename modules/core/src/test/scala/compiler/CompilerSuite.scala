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

import cats.data.NonEmptyChain
import cats.implicits._
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import Query._
import Predicate._, Value._, UntypedOperation._
import QueryCompiler._, ComponentElaborator.TrivialJoin

final class CompilerSuite extends CatsEffectSuite {
  val queryParser = QueryParser(GraphQLParser(GraphQLParser.defaultConfig))

  test("simple query") {
    val query = """
      query {
        character(id: "1000") {
          name
        }
      }
    """

    val expected =
      UntypedSelect("character", None, List(Binding("id", StringValue("1000"))), Nil,
        UntypedSelect("name", None, Nil, Nil, Empty)
      )

    val res = queryParser.parseText(query).map(_._1)
    assertEquals(res, Result.Success(List(UntypedQuery(None, expected, Nil, Nil))))
  }

  test("simple mutation") {
    val query = """
      mutation {
        update_character(id: "1000", name: "Luke") {
          character {
            name
          }
        }
      }
    """

    val expected =
      UntypedSelect("update_character", None, List(Binding("id", StringValue("1000")), Binding("name", StringValue("Luke"))), Nil,
        UntypedSelect("character", None, Nil, Nil,
          UntypedSelect("name", None, Nil, Nil, Empty)
        )
      )

    val res = queryParser.parseText(query).map(_._1)
    assertEquals(res, Result.Success(List(UntypedMutation(None, expected, Nil, Nil))))
  }

  test("simple subscription") {
    val query = """
      subscription {
        character(id: "1000") {
          name
        }
      }
    """

    val expected =
      UntypedSelect("character", None, List(Binding("id", StringValue("1000"))), Nil,
        UntypedSelect("name", None, Nil, Nil, Empty)
      )

    val res = queryParser.parseText(query).map(_._1)
    assertEquals(res, Result.Success(List(UntypedSubscription(None, expected, Nil, Nil))))
  }

  test("simple nested query") {
    val query = """
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
      UntypedSelect(
        "character", None, List(Binding("id", StringValue("1000"))), Nil,
        UntypedSelect("name", None, Nil, Nil, Empty) ~
          UntypedSelect(
            "friends", None, Nil, Nil,
            UntypedSelect("name", None, Nil, Nil, Empty)
          )
      )

    val res = queryParser.parseText(query).map(_._1)
    assertEquals(res, Result.Success(List(UntypedQuery(None, expected, Nil, Nil))))
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
      UntypedSelect(
        "hero", None, List(Binding("episode", EnumValue("NEWHOPE"))), Nil,
        UntypedSelect("name", None, Nil, Nil, Empty) ~
        UntypedSelect("friends", None, Nil, Nil,
          UntypedSelect("name", None, Nil, Nil, Empty) ~
          UntypedSelect("friends", None, Nil, Nil,
            UntypedSelect("name", None, Nil, Nil, Empty)
          )
        )
      )

    val res = queryParser.parseText(query).map(_._1)
    assertEquals(res, Result.Success(List(UntypedQuery(None, expected, Nil, Nil))))
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
      UntypedSelect("user", None, List(Binding("id", IntValue(4))), Nil,
        Group(List(
          UntypedSelect("id", None, Nil, Nil, Empty),
          UntypedSelect("name", None, Nil, Nil, Empty),
          UntypedSelect("profilePic", Some("smallPic"), List(Binding("size", IntValue(64))), Nil, Empty),
          UntypedSelect("profilePic", Some("bigPic"), List(Binding("size", IntValue(1024))), Nil, Empty)
        ))
      )

    val res = queryParser.parseText(query).map(_._1)
    assertEquals(res, Result.Success(List(UntypedQuery(None, expected, Nil, Nil))))
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
      UntypedSelect(
        "__schema", None, Nil, Nil,
        UntypedSelect("queryType", None, Nil, Nil, UntypedSelect("name", None, Nil, Nil, Empty)) ~
        UntypedSelect("mutationType", None, Nil, Nil, UntypedSelect("name", None, Nil, Nil, Empty)) ~
        UntypedSelect("subscriptionType", None, Nil, Nil, UntypedSelect("name", None, Nil, Nil, Empty))
      )

    val res = queryParser.parseText(query).map(_._1)
    assertEquals(res, Result.Success(List(UntypedQuery(Some("IntrospectionQuery"), expected, Nil, Nil))))
  }

  test("simple selector elaborated query") {
    val query = """
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
        "character", None,
        Unique(
          Filter(Eql(AtomicMapping.CharacterType / "id", Const("1000")),
          Select("name") ~
              Select(
                "friends",
                Select("name")
              )
          )
        )
    )

    val res = AtomicMapping.compiler.compile(query)

    assertEquals(res.map(_.query), Result.Success(expected))
  }

  test("invalid: object subselection set empty") {
    val query = """
      query {
        character(id: "1000")
      }
    """

    val expected = Problem("Non-leaf field 'character' of Query must have a non-empty subselection set")

    val res = AtomicMapping.compiler.compile(query)

    assertEquals(res, Result.Failure(NonEmptyChain(expected)))
  }

  test("invalid: object subselection set invalid") {
    val query = """
      query {
        character(id: "1000") {
          foo
        }
      }
    """

    val expected = Problem("No field 'foo' for type Character")

    val res = AtomicMapping.compiler.compile(query)

    assertEquals(res, Result.Failure(NonEmptyChain(expected)))
  }

  test("invalid: leaf subselection set not empty (1)") {
    val query = """
      query {
        character(id: "1000") {
          name {
            __typename
          }
        }
      }
    """

    val expected = Problem("Leaf field 'name' of Character must have an empty subselection set")

    val res = AtomicMapping.compiler.compile(query)

    assertEquals(res, Result.Failure(NonEmptyChain(expected)))
  }

  test("invalid: leaf subselection set not empty (2)") {
    val query = """
      query {
        character(id: "1000") {
          name {
            foo
          }
        }
      }
    """

    val expected = Problem("Leaf field 'name' of Character must have an empty subselection set")

    val res = AtomicMapping.compiler.compile(query)

    assertEquals(res, Result.Failure(NonEmptyChain(expected)))
  }

  test("invalid: bogus field argument") {
    val query = """
      query {
        character(id: "1000", quux: 23) {
          name
        }
      }
    """

    val expected = Problem("Unknown argument(s) 'quux' in field character of type Query")

    val res = AtomicMapping.compiler.compile(query)

    assertEquals(res, Result.Failure(NonEmptyChain(expected)))
  }

  test("simple component elaborated query") {
    val query = """
      query {
        componenta {
          fielda1
          fielda2 {
            componentb {
              fieldb1
              fieldb2 {
                componentc {
                  fieldc1
                }
              }
            }
          }
        }
      }
    """

    val expected =
      Component(ComponentA, TrivialJoin,
        Select("componenta",
          Select("fielda1") ~
          Select("fielda2",
            Component(ComponentB, TrivialJoin,
              Select("componentb",
                Select("fieldb1") ~
                Select("fieldb2",
                  Component(ComponentC, TrivialJoin,
                    Select("componentc",
                      Select("fieldc1")
                    )
                  )
                )
              )
            )
          )
        )
      )

    val res = ComposedMapping.compiler.compile(query)

    assertEquals(res.map(_.query), Result.Success(expected))
  }

  test("malformed query (1)") {
    val query =
      """|query {
         |  character(id: "1000" {
         |    name
         |  }
         |}""".stripMargin

    val expected =
      """|query {
         |  character(id: "1000" {
         |                       ^
         |expectation:
         |* must be char: ')'
         |    name
         |  }""".stripMargin

    val res = queryParser.parseText(query)

    assertEquals(res, Result.failure(expected))
  }

  test("malformed query (2)") {
    val query = ""

    val res = queryParser.parseText(query)

    assertEquals(res, Result.failure("At least one operation required"))
  }

  test("malformed query (3)") {
    val query =
      """|query {
         |  character(id: "1000") {
         |    name
         |  }""".stripMargin

    val expected =
      """|...
         |  character(id: "1000") {
         |    name
         |  }
         |   ^
         |expectation:
         |* must be char: '}'""".stripMargin

    val res = queryParser.parseText(query)
    //println(res.toProblems.toList.head.message)

    assertEquals(res, Result.failure(expected))
  }
}

object AtomicMapping extends TestMapping {
  val schema =
    schema"""
      type Query {
        character(id: String!): Character
      }
      type Character {
        id: String!
        name: String
        friends: [Character!]
      }
    """

  val QueryType = schema.ref("Query")
  val CharacterType = schema.ref("Character")

  override val selectElaborator = SelectElaborator {
    case (QueryType, "character", List(Binding("id", StringValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CharacterType / "id", Const(id)), child)))
  }
}
trait DummyComponent extends TestMapping {
  val schema = schema"type Query { dummy: Int }"
}

object ComponentA extends DummyComponent
object ComponentB extends DummyComponent
object ComponentC extends DummyComponent

object ComposedMapping extends TestMapping {
  val schema =
    schema"""
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
        fieldc1: Int
      }
    """

  val QueryType = schema.ref("Query")
  val FieldA2Type = schema.ref("FieldA2")
  val FieldB2Type = schema.ref("FieldB2")

  override val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            Delegate("componenta", ComponentA)
          )
      ),
      ObjectMapping(
        tpe = FieldA2Type,
        fieldMappings =
          List(
            Delegate("componentb", ComponentB)
          )
      ),
      ObjectMapping(
        tpe = FieldB2Type,
        fieldMappings =
          List(
            Delegate("componentc", ComponentC)
          )
      )
    )
}
