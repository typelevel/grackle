// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.Id
import cats.data.Ior
import cats.tests.CatsSuite

import edu.gemini.grackle._
import Query._, Value._
import QueryCompiler._
import edu.gemini.grackle.Operation

final class InputValuesSuite extends CatsSuite {
  test("null value") {
    val query = """
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
      Operation(
        Group(List(
          Select("field", List(Binding("arg", AbsentValue)), Select("subfield", Nil, Empty)),
          Select("field", List(Binding("arg", NullValue)), Select("subfield", Nil, Empty)),
          Select("field", List(Binding("arg", IntValue(23))), Select("subfield", Nil, Empty))
        )),
        InputValuesMapping.schema.queryType
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }

  test("list value") {
    val query = """
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
      Operation(
        Group(List(
          Select("listField", List(Binding("arg", ListValue(Nil))),
            Select("subfield", Nil, Empty)
          ),
          Select("listField", List(Binding("arg", ListValue(List(StringValue("foo"),  StringValue("bar"))))),
            Select("subfield", Nil, Empty)
          )
        )),
        InputValuesMapping.schema.queryType
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }

  test("input object value") {
    val query = """
      query {
        objectField(arg: { foo: 23, bar: true, baz: "quux" }) {
          subfield
        }
      }
    """

    val expected =
      Operation(
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
        ),
        InputValuesMapping.schema.queryType
      )

    val compiled = InputValuesMapping.compiler.compile(query, None)
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }
}

object InputValuesMapping extends Mapping[Id] {
  val schema =
    Schema(
      """
        type Query {
          field(arg: Int): Result!
          listField(arg: [String!]!): Result!
          objectField(arg: InObj!): Result!
        }
        type Result {
          subfield: String!
        }
        input InObj {
          foo: Int!
          bar: Boolean!
          baz: String!
          defaulted: String! = "quux"
          nullable: String
        }
      """
    ).right.get

  val QueryType = schema.ref("Query")

  val typeMappings = Nil

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> PartialFunction.empty
  ))
}
