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
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import Query._
import Value._

final class VariablesSuite extends CatsEffectSuite {
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
      UntypedSelect("user", None, List(Binding("id", IDValue("4"))), Nil,
        Group(List(
          UntypedSelect("id", None, Nil, Nil, Empty),
          UntypedSelect("name", None, Nil, Nil, Empty),
          UntypedSelect("profilePic", None, List(Binding("size", IntValue(60))), Nil, Empty)
        ))
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
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
      UntypedSelect("users", None,
        List(Binding("ids", ListValue(List(IDValue("1"), IDValue("2"), IDValue("3"))))),
        Nil,
        UntypedSelect("name", None, Nil, Nil, Empty)
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
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
      UntypedSelect("usersByType", None,
        List(Binding("userType", EnumValue("ADMIN"))),
        Nil,
        UntypedSelect("name", None, Nil, Nil, Empty)
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
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
      UntypedSelect("usersLoggedInByDate", None,
        List(Binding("date", StringValue("2021-02-22"))),
        Nil,
        UntypedSelect("name", None, Nil, Nil, Empty)
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
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
      UntypedSelect("queryWithBigDecimal", None,
        List(Binding("input", IntValue(2021))),
        Nil,
        UntypedSelect("name", None, Nil, Nil, Empty)
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
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
      UntypedSelect("search", None,
        List(Binding("pattern",
          ObjectValue(List(
            ("name", StringValue("Foo")),
            ("age", IntValue(23)),
            ("id", IDValue("123")),
            ("userType", AbsentValue),
            ("date", AbsentValue)
          ))
        )),
        Nil,
        Group(List(
          UntypedSelect("name", None, Nil, Nil, Empty),
          UntypedSelect("id", None, Nil, Nil, Empty)
        ))
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
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

    val expected = Problem("Unknown field(s) 'quux' in input object value of type Pattern in variable values")

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Failure(NonEmptyChain.one(expected)))
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
      UntypedSelect("users", None,
        List(Binding("ids", ListValue(List(IDValue("1"), IDValue("2"), IDValue("3"))))),
        Nil,
        UntypedSelect("name", None, Nil, Nil, Empty)
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
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
      UntypedSelect("search", None,
        List(Binding("pattern",
          ObjectValue(List(
            ("name", StringValue("Foo")),
            ("age", IntValue(23)),
            ("id", IDValue("123")),
            ("userType", AbsentValue),
            ("date", AbsentValue)
          ))
        )),
        Nil,
        Group(List(
          UntypedSelect("name", None, Nil, Nil, Empty),
          UntypedSelect("id", None, Nil, Nil, Empty)
        ))
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
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
      UntypedSelect("search", None,
        List(Binding("pattern",
          ObjectValue(List(
            ("name", StringValue("Foo")),
            ("age", IntValue(23)),
            ("id", IDValue("123")),
            ("userType", EnumValue("ADMIN")),
            ("date", AbsentValue)
          ))
        )),
        Nil,
        Group(List(
          UntypedSelect("name", None, Nil, Nil, Empty),
          UntypedSelect("id", None, Nil, Nil, Empty)
        ))
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
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
      UntypedSelect("search", None,
        List(Binding("pattern",
          ObjectValue(List(
            ("name", StringValue("Foo")),
            ("age", IntValue(23)),
            ("id", IDValue("123")),
            ("userType", AbsentValue),
            ("date", StringValue("2021-02-22"))
          ))
        )),
        Nil,
        Group(List(
          UntypedSelect("name", None, Nil, Nil, Empty),
          UntypedSelect("id", None, Nil, Nil, Empty)
        ))
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("variables in directive argument") {
    val query = """
      query getZuckProfile($skipName: Boolean) {
        user(id: 4) {
          id
          name @skip(if: $skipName)
        }
      }
    """

    val variables = json"""
      {
        "skipName": true
      }
    """

    val expected =
      UntypedSelect("user", None, List(Binding("id", IDValue("4"))), Nil,
        UntypedSelect("id", None, Nil, Nil, Empty)
      )

    val compiled = VariablesMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("variable not defined (1)") {
    val query = """
      query getZuckProfile {
        user(id: 4) {
          id
          name
          profilePic(size: $devicePicSize)
        }
      }
    """

    val compiled = VariablesMapping.compiler.compile(query)

    val expected = Result.failure("Variable 'devicePicSize' is undefined")

    assertEquals(compiled, expected)
  }


  test("variable not defined (2)") {
    val query = """
      query getZuckProfile($devicePicSize: Int) {
        user(id: 4) {
          id
          name
          profilePic(size: $devicePicSize)
        }
      }
    """

    val compiled = VariablesMapping.compiler.compile(query)

    val expected = Result.failure("Variable 'devicePicSize' is undefined")

    assertEquals(compiled, expected)
  }

  test("variable not defined (3)") {
    val query = """
      query getZuckProfile($skipPic: Boolean) {
        user(id: 4) {
          id
          name
          profilePic(size: $devicePicSize) @skip(if: $skipPic)
        }
      }
    """

    val expected = Result.failure("Variable 'devicePicSize' is undefined")

    val compiled = VariablesMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), expected)
  }

  test("variable not defined (4)") {
    val query = """
      query getZuckProfile {
        user(id: 4) {
          id
          name @skip(if: $skipName)
        }
      }
    """

    val expected = Result.failure("Variable 'skipName' is undefined")

    val compiled = VariablesMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), expected)
  }


  test("variable not defined (5)") {
    val query = """
      query getZuckProfile($skipName: Boolean) {
        user(id: 4) {
          id
          name @skip(if: $skipName)
        }
      }
    """

    val expected = Result.failure("Variable 'skipName' is undefined")

    val compiled = VariablesMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), expected)
  }

  test("variable unused (1)") {
    val query = """
      query getZuckProfile($devicePicSize: Int) {
        user(id: 4) {
          id
          name
        }
      }
    """

    val compiled = VariablesMapping.compiler.compile(query)

    val expected = Result.failure("Variable 'devicePicSize' is unused")

    assertEquals(compiled, expected)
  }
}

object VariablesMapping extends TestMapping {
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

  override val selectElaborator = PreserveArgsElaborator
}
