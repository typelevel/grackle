// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.data.Ior
import cats.implicits._
import cats.tests.CatsSuite

import edu.gemini.grackle._
import Query._, Binding._, Predicate._
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
      Select("character", List(StringBinding("id", "1000")),
        Select("name", Nil)
      )

    val res = QueryParser.compileText(text)
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

    val res = QueryParser.compileText(text)
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

object SimpleSchema extends Schema {
  import ScalarType._

  val IdArg = InputValue("id", None, StringType, None)

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("character", None, List(IdArg), NullableType(TypeRef("Character")), false, None),
      ),
      interfaces = Nil
    ),
    InterfaceType(
      name = "Character",
      description = None,
      fields = List(
        Field("id", None, Nil, StringType, false, None),
        Field("name", None, Nil, NullableType(StringType), false, None),
        Field("friends", None, Nil, NullableType(ListType(TypeRef("Character"))), false, None),
      )
    )
  )

  val directives = Nil
}

object SimpleCompiler extends QueryCompiler(SimpleSchema) {
  val selectElaborator = new SelectElaborator(Map(
    SimpleSchema.tpe("Query").dealias -> {
      case Select("character", List(StringBinding("id", id)), child) =>
        Unique(FieldEquals("id", id), child).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

object SchemaA extends Schema {
  import ScalarType._

  val types = List(
    ObjectType(
      name = "FieldA2",
      description = None,
      fields = Nil,
      interfaces = Nil
    ),
    ObjectType(
      name = "ComponentA",
      description = None,
      fields = List(
        Field("fielda1", None, Nil, StringType, false, None),
        Field("fielda2", None, Nil, TypeRef("FieldA2"), false, None)
      ),
      interfaces = Nil
    )
  )

  val directives = Nil
}

object SchemaB extends Schema {
  import ScalarType._

  val types = List(
    ObjectType(
      name = "FieldB2",
      description = None,
      fields = Nil,
      interfaces = Nil
    ),
    ObjectType(
      name = "ComponentB",
      description = None,
      fields = List(
        Field("fieldb1", None, Nil, StringType, false, None),
        Field("fieldb2", None, Nil, TypeRef("FieldB2"), false, None)
      ),
      interfaces = Nil
    )
  )

  val directives = Nil
}

object SchemaC extends Schema {
  val types = List(
    ObjectType(
      name = "ComponentC",
      description = None,
      fields = Nil,
      interfaces = Nil
    )
  )

  val directives = Nil
}

object ComposedSchema extends Schema {
  import ScalarType._

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("componenta", None, Nil, TypeRef("ComponentA"), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "ComponentA",
      description = None,
      fields = List(
        Field("fielda1", None, Nil, StringType, false, None),
        Field("fielda2", None, Nil, TypeRef("FieldA2"), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "FieldA2",
      description = None,
      fields = List(
        Field("componentb", None, Nil, TypeRef("ComponentB"), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "ComponentB",
      description = None,
      fields = List(
        Field("fieldb1", None, Nil, StringType, false, None),
        Field("fieldb2", None, Nil, TypeRef("FieldB2"), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "FieldB2",
      description = None,
      fields = List(
        Field("componentc", None, Nil, TypeRef("ComponentC"), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "ComponentC",
      description = None,
      fields = Nil,
      interfaces = Nil
    )
  )

  val directives = Nil
}

object ComposedCompiler extends QueryCompiler(ComposedSchema) {
  val componentElaborator = ComponentElaborator(
    Mapping(ComposedSchema.tpe("Query"), "componenta", "ComponentA"),
    Mapping(ComposedSchema.tpe("FieldA2"), "componentb", "ComponentB"),
    Mapping(ComposedSchema.tpe("FieldB2"), "componentc", "ComponentC")
  )

  val phases = List(componentElaborator)
}
