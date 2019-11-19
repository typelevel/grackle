// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.data.Ior
import cats.tests.CatsSuite

import edu.gemini.grackle._
import Query._, Binding._, Predicate._

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
      Select("character", List(StringBinding("id", "1000")),
        Select("name", Nil)
      )

    val res = Compiler.compileText(text)
    assert(res == Ior.Right(expected))
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
        "character", List(StringBinding("id", "1000")),
        Select("name", Nil) ~
          Select(
            "friends", Nil,
            Select("name", Nil)
          )
      )

    val res = Compiler.compileText(text)
    assert(res == Ior.Right(expected))
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
      Unique(FieldEquals("id", "1000"),
        Select("name", Nil) ~
          Select(
            "friends", Nil,
            Select("name", Nil)
          )
      )

    import SimpleSchema._

    val elaborator = new SelectElaborator(Map(
      QueryType -> {
        case Select("character", List(StringBinding("id", id)), child) =>
          Unique(FieldEquals("id", id), child).rightIor
      }
    ))

    val query: Result[Query] = Compiler.compileText(text)
    val res: Result[Query]  = query.flatMap((q: Query) => elaborator(q, QueryType))

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

    val dummyJoin = (_: Cursor, q: Query) => q.rightIor

    val expected =
      Wrap("componenta",
        Component(SchemaA, dummyJoin,
          Select("fielda1", Nil) ~
          Select("fielda2", Nil,
            Wrap("componentb",
              Component(SchemaB, dummyJoin,
                Select("fieldb1", Nil) ~
                Select("fieldb2", Nil,
                  Wrap("componentc",
                    Component(SchemaC, dummyJoin, Empty)
                  )
                )
              )
            )
          )
        )
      )

    import CompositeSchema._
    import SchemaA._
    import SchemaB._
    import SchemaC._

    val elaborator = new ComponentElaborator(Map(
      (QueryType, "componenta")   -> ((SchemaA, ComponentAType, dummyJoin)),
      (FieldA2Type, "componentb") -> ((SchemaB, ComponentBType, dummyJoin)),
      (FieldB2Type, "componentc") -> ((SchemaC, ComponentCType, dummyJoin))
    ))

    val query: Result[Query] = Compiler.compileText(text)
    val res: Result[Query]  = query.flatMap((q: Query) => elaborator(q, QueryType))

    assert(res == Ior.Right(expected))
  }
}

object SimpleSchema extends Schema {
  import ScalarType._

  val IdArg = InputValue("id", None, StringType, None)

  val QueryType: ObjectType =
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("character", None, List(IdArg), NullableType(TypeRef("Character")), false, None),
      ),
      interfaces = Nil
    )

  val CharacterType: InterfaceType =
    InterfaceType(
      name = "Character",
      description = None,
      fields = List(
        Field("id", None, Nil, StringType, false, None),
        Field("name", None, Nil, NullableType(StringType), false, None),
        Field("friends", None, Nil, NullableType(ListType(TypeRef("Character"))), false, None),
      )
    )

  val types = List(QueryType, CharacterType)
  val queryType = TypeRef("Query")
  val mutationType = None
  val subscriptionType = None
  val directives = Nil
}

object SchemaA extends SchemaComponent {
  import ScalarType._

  val FieldA2Type: ObjectType =
    ObjectType(
      name = "FieldA2",
      description = None,
      fields = Nil,
      interfaces = Nil
    )

  val ComponentAType: ObjectType =
    ObjectType(
      name = "ComponentA",
      description = None,
      fields = List(
        Field("fielda1", None, Nil, StringType, false, None),
        Field("fielda2", None, Nil, TypeRef("FieldA2"), false, None)
      ),
      interfaces = Nil
    )

  val types = List(ComponentAType, FieldA2Type)
}

object SchemaB extends SchemaComponent {
  import ScalarType._

  val FieldB2Type: ObjectType =
    ObjectType(
      name = "FieldB2",
      description = None,
      fields = Nil,
      interfaces = Nil
    )

  val ComponentBType: ObjectType =
    ObjectType(
      name = "ComponentB",
      description = None,
      fields = List(
        Field("fieldb1", None, Nil, StringType, false, None),
        Field("fieldb2", None, Nil, TypeRef("FieldB2"), false, None)
      ),
      interfaces = Nil
    )

  val types = List(ComponentBType, FieldB2Type)
}

object SchemaC extends SchemaComponent {
  val ComponentCType: ObjectType =
    ObjectType(
      name = "ComponentC",
      description = None,
      fields = Nil,
      interfaces = Nil
    )

  val types = List(ComponentCType)
}

object CompositeSchema extends Schema {
  val QueryType =
    ObjectType(
      name = "Query",
      description = None,
      fields = Nil,
      interfaces = Nil
    )

  val types = List(QueryType) ++ SchemaA.types ++ SchemaB.types ++ SchemaC.types
  val queryType = TypeRef("Query")
  val mutationType = None
  val subscriptionType = None
  val directives = Nil
}
