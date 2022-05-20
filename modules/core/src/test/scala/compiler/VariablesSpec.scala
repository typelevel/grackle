// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.Id
import cats.data.{Chain, Ior}
import cats.tests.CatsSuite

import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import Query._, Value._
import QueryCompiler._

final class VariablesSuite extends CatsSuite {
  test("simple variables query") {
    val query = """
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

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)
    assert(compiled.map(_.query) == Ior.Right(expected))
  }

  test("list variable query") {
    val query = """
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

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)
    assert(compiled.map(_.query) == Ior.Right(expected))
  }

  test("enum variable query") {
    val query = """
      query getUserType($userType: UserType) {
        usersByType(userType: $userType) {
          name
        }
      }
    """

    val variables = json"""
      {
        "userType": "ADMIN"
      }
    """

    val expected =
      Select("usersByType",
        List(Binding("userType", TypedEnumValue(EnumValue("ADMIN", None, false, None)))),
        Select("name", Nil, Empty)
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)
    assert(compiled.map(_.query) == Ior.Right(expected))
  }

  test("scalar variable query") {
    val query = """
      query getLoggedInByDate($date: Date) {
        usersLoggedInByDate(date: $date) {
          name
        }
      }
    """

    val variables = json"""
      {
        "date": "2021-02-22"
      }
    """

    val expected =
      Select("usersLoggedInByDate",
        List(Binding("date", StringValue("2021-02-22"))),
        Select("name", Nil, Empty)
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)
    assert(compiled.map(_.query) == Ior.Right(expected))
  }

  test("scalar variable query bigdecimal") {
    val query = """
      query queryWithBigDecimal($input: BigDecimal) {
        queryWithBigDecimal(input: $input) {
          name
        }
      }
    """

    val variables = json"""
      {
        "input": 2021
      }
    """

    val expected =
      Select("queryWithBigDecimal",
        List(Binding("input", IntValue(2021))),
        Select("name", Nil, Empty)
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)
    assert(compiled.map(_.query) == Ior.Right(expected))
  }

  test("object variable query") {
    val query = """
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
            ("id", IDValue("123")),
            ("userType", AbsentValue),
            ("date", AbsentValue)
          ))
        )),
        Group(List(
          Select("name", Nil, Empty),
          Select("id", Nil, Empty)
        ))
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)
    assert(compiled.map(_.query) == Ior.Right(expected))
  }

  test("invalid: bogus input object field") {
    val query = """
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
          "id": 123,
          "quux": 23
        }
      }
    """

    val expected = Problem("Unknown field(s) 'quux' in input object value of type Pattern")

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)
    assert(compiled.map(_.query) == Ior.Left(Chain.one(expected)))
  }

  test("variable within list query") {
    val query = """
      query getProfile($id: ID!) {
        users(ids: [1, $id, 3]) {
          name
        }
      }
    """

    val variables = json"""
      {
        "id": 2
      }
    """

    val expected =
      Select("users",
        List(Binding("ids", ListValue(List(IDValue("1"), IDValue("2"), IDValue("3"))))),
        Select("name", Nil, Empty)
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)
    assert(compiled.map(_.query) == Ior.Right(expected))
  }

  test("simple variable within an object query") {
    val query = """
      query doSearch($age: Int) {
        search(pattern: { name: "Foo", age: $age, id: 123} ) {
          name
          id
        }
      }
    """

    val variables = json"""
      {
        "age": 23
      }
    """

    val expected =
      Select("search",
        List(Binding("pattern",
          ObjectValue(List(
            ("name", StringValue("Foo")),
            ("age", IntValue(23)),
            ("id", IDValue("123")),
            ("userType", AbsentValue),
            ("date", AbsentValue)
          ))
        )),
        Group(List(
          Select("name", Nil, Empty),
          Select("id", Nil, Empty)
        ))
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)
    assert(compiled.map(_.query) == Ior.Right(expected))
  }

  test("enum variable within an object query") {
    val query = """
      query doSearch($userType: UserType) {
        search(pattern: { name: "Foo", age: 23, id: 123, userType: $userType} ) {
          name
          id
        }
      }
    """

    val variables = json"""
      {
        "userType": "ADMIN"
      }
    """

    val expected =
      Select("search",
        List(Binding("pattern",
          ObjectValue(List(
            ("name", StringValue("Foo")),
            ("age", IntValue(23)),
            ("id", IDValue("123")),
            ("userType", TypedEnumValue(EnumValue("ADMIN", None, false, None))),
            ("date", AbsentValue)
          ))
        )),
        Group(List(
          Select("name", Nil, Empty),
          Select("id", Nil, Empty)
        ))
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)
    assert(compiled.map(_.query) == Ior.Right(expected))
  }

  test("scalar variable within an object query") {
    val query = """
      query doSearch($date: Date) {
        search(pattern: { name: "Foo", age: 23, id: 123, date: $date} ) {
          name
          id
        }
      }
    """

    val variables = json"""
      {
        "date": "2021-02-22"
      }
    """

    val expected =
      Select("search",
        List(Binding("pattern",
          ObjectValue(List(
            ("name", StringValue("Foo")),
            ("age", IntValue(23)),
            ("id", IDValue("123")),
            ("userType", AbsentValue),
            ("date", StringValue("2021-02-22"))
          ))
        )),
        Group(List(
          Select("name", Nil, Empty),
          Select("id", Nil, Empty)
        ))
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)
    assert(compiled.map(_.query) == Ior.Right(expected))
  }
}

object VariablesMapping extends Mapping[Id] {
  val schema =
    schema"""
      type Query {
        user(id: ID!): User!
        users(ids: [ID!]!): [User!]!
        search(pattern: Pattern!): [User!]!
        usersByType(userType: UserType!): [User!]!
        usersLoggedInByDate(date: Date!): [User!]!
        queryWithBigDecimal(input: BigDecimal!): [User!]!
      }
      type User {
        id: String!
        name: String!
        profilePic(size: Int): String!
      }
      input Pattern {
        name: String
        age: Int
        id: ID
        userType: UserType
        date: Date
      }
      enum UserType {
        ADMIN
        NORMAL
      }
      scalar Date
      scalar BigDecimal
    """

  val typeMappings = Nil

  val QueryType = schema.ref("Query")

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> PartialFunction.empty
  ))
}
