// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.data.Ior
import cats.tests.CatsSuite

import edu.gemini.grackle._
import Query._, Value._
import QueryCompiler._

final class InputValuesSuite extends CatsSuite {
  test("null value") {
    val text = """
      query {
        field {
          subfield
        }
        field(arg: null) {
          subfield
        }
        field(arg: 23) {
          subfield
        }
      }
    """

    val expected =
      Group(List(
        Select("field", List(Binding("arg", AbsentValue)), Select("subfield", Nil, Empty)),
        Select("field", List(Binding("arg", NullValue)), Select("subfield", Nil, Empty)),
        Select("field", List(Binding("arg", IntValue(23))), Select("subfield", Nil, Empty))
      ))

    val compiled = InputValuesCompiler.compile(text, None)
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }

  test("list value") {
    val text = """
      query {
        listField(arg: []) {
          subfield
        }
        listField(arg: ["foo", "bar"]) {
          subfield
        }
      }
    """

    val expected =
      Group(List(
        Select("listField", List(Binding("arg", ListValue(Nil))),
          Select("subfield", Nil, Empty)
        ),
        Select("listField", List(Binding("arg", ListValue(List(StringValue("foo"),  StringValue("bar"))))),
          Select("subfield", Nil, Empty)
        )
      ))

    val compiled = InputValuesCompiler.compile(text, None)
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }

  test("input object value") {
    val text = """
      query {
        objectField(arg: { foo: 23, bar: true, baz: "quux" }) {
          subfield
        }
      }
    """

    val expected =
      Select("objectField",
        List(Binding("arg",
          ObjectValue(List(
            ("foo", IntValue(23)),
            ("bar", BooleanValue(true)),
            ("baz", StringValue("quux")),
            ("defaulted", StringValue("quux")),
            ("nullable", AbsentValue)
          ))
        )),
        Select("subfield", Nil, Empty)
      )

    val compiled = InputValuesCompiler.compile(text, None)
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }
}

object InputValuesSchema extends Schema {
  import ScalarType._

  val ArgArg = InputValue("arg", None, NullableType(IntType), None)
  val ListArgArg = InputValue("arg", None, ListType(StringType), None)
  val ObjectArgArg = InputValue("arg", None, TypeRef("InObj"), None)

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("field", None, List(ArgArg), TypeRef("Result"), false, None),
        Field("listField", None, List(ListArgArg), TypeRef("Result"), false, None),
        Field("objectField", None, List(ObjectArgArg), TypeRef("Result"), false, None),
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "Result",
      description = None,
      fields = List(
        Field("subfield", None, Nil, StringType, false, None),
      ),
      interfaces = Nil
    ),
    InputObjectType(
      name = "InObj",
      description = None,
      inputFields = List(
        InputValue("foo", None, IntType, None),
        InputValue("bar", None, BooleanType, None),
        InputValue("baz", None, StringType, None),
        InputValue("defaulted", None, StringType, Some(StringValue("quux"))),
        InputValue("nullable", None, NullableType(StringType), None)
      )
    )
  )

  val directives = Nil
}

object InputValuesCompiler extends QueryCompiler(InputValuesSchema) {
  val selectElaborator = new SelectElaborator(Map(
    InputValuesSchema.tpe("Query").dealias -> PartialFunction.empty
  ))

  val phases = List(selectElaborator)
}
