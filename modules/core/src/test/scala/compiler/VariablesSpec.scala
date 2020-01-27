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
}

object VariablesSchema extends Schema {
  import ScalarType._

  val IdArg = InputValue("id", None, IDType, None)
  val SizeArg = InputValue("size", None, NullableType(IntType), None)

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("user", None, List(IdArg), TypeRef("User"), false, None),
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
