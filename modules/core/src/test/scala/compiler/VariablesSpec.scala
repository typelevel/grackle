// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.data.Ior
import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import edu.gemini.grackle._
import Query._, Value._
import QueryCompiler._

final class VariablesSuite extends CatsSuite {
  test("simple variables query") {
    val text = """
      query getZuckProfile($devicePicSize: Int) {
        user(id: 4) {
          id
          name
          profilePic(size: $devicePicSize)
        }
      }
    """

    val variables = json"""
      {
        "devicePicSize": 60
      }
    """

    val expected =
      Select("user", List(Binding("id", IDValue("4"))),
        Group(List(
          Select("id", Nil, Empty),
          Select("name", Nil, Empty),
          Select("profilePic", List(Binding("size", IntValue(60))), Empty)
        ))
      )

    val compiled = VariablesCompiler.compile(text, Some(variables))
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }

  test("list variable query") {
    val text = """
      query getProfile($ids: [ID!]) {
        users(ids: $ids) {
          name
        }
      }
    """

    val variables = json"""
      {
        "ids": [1, 2, 3]
      }
    """

    val expected =
      Select("users",
        List(Binding("ids", ListValue(List(IDValue("1"), IDValue("2"), IDValue("3"))))),
        Select("name", Nil, Empty)
      )

    val compiled = VariablesCompiler.compile(text, Some(variables))
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }

  test("object variable query") {
    val text = """
      query doSearch($pattern: Pattern) {
        search(pattern: $pattern) {
          name
          id
        }
      }
    """

    val variables = json"""
      {
        "pattern": {
          "name": "Foo",
          "age": 23,
          "id": 123
        }
      }
    """

    val expected =
      Select("search",
        List(Binding("pattern",
          ObjectValue(List(
            ("name", StringValue("Foo")),
            ("age", IntValue(23)),
            ("id", IDValue("123"))
          ))
        )),
        Group(List(
          Select("name", Nil, Empty),
          Select("id", Nil, Empty)
        ))
      )

    val compiled = VariablesCompiler.compile(text, Some(variables))
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }
}

object VariablesSchema extends Schema {
  import ScalarType._

  val IdArg = InputValue("id", None, IDType, None)
  val IdsArg = InputValue("ids", None, ListType(IDType), None)
  val SizeArg = InputValue("size", None, NullableType(IntType), None)
  val PatternArg = InputValue("pattern", None, TypeRef("Pattern"), None)

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("user", None, List(IdArg), TypeRef("User"), false, None),
        Field("users", None, List(IdsArg), ListType(TypeRef("User")), false, None),
        Field("search", None, List(PatternArg), ListType(TypeRef("User")), false, None),
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "User",
      description = None,
      fields = List(
        Field("id", None, Nil, StringType, false, None),
        Field("name", None, Nil, StringType, false, None),
        Field("profilePic", None, List(SizeArg), StringType, false, None),
      ),
      interfaces = Nil
    ),
    InputObjectType(
      name = "Pattern",
      description = None,
      inputFields = List(
        InputValue("name", None, NullableType(StringType), None),
        InputValue("age", None, NullableType(IntType), None),
        InputValue("id", None, NullableType(IDType), None),
      )
    )
  )

  val directives = Nil
}

object VariablesCompiler extends QueryCompiler(VariablesSchema) {
  val selectElaborator = new SelectElaborator(Map(
    VariablesSchema.tpe("Query").dealias -> PartialFunction.empty
  ))

  val phases = List(selectElaborator)
}
