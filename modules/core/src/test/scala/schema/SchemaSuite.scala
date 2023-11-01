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

package schema

import cats.data.NonEmptyChain
import munit.CatsEffectSuite

import grackle.{Result, Schema}
import grackle.syntax._

final class SchemaSuite extends CatsEffectSuite {
  test("schema validation: undefined types: typo in the use of a Query result type") {
    val schema =
      Schema(
        """
          type Query {
            episodeById(id: String!): Episod
          }

          type Episode {
            id: String!
          }
        """
      )

    schema match {
      case Result.Failure(ps) => assertEquals(ps.map(_.message), NonEmptyChain("Reference to undefined type 'Episod'"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: undefined types: typo in the use of an InputValueDefinition") {
    val schema = Schema(
      """
        type Query {
          episodeById(id: CCid!): Episode
        }

        scalar CCId

        type Episode {
          id: CCId!
        }
      """
    )

    schema match {
      case Result.Failure(ps)  =>
        assertEquals(ps.map(_.message), NonEmptyChain("Reference to undefined type 'CCid'"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: multiply-defined types") {
    val schema = Schema(
      """
        type Query {
          episodeById(id: String!): Episode
        }

        type Episode {
          id: String!
        }

        type Episode {
          episodeId: String!
        }
      """
    )

    schema match {
      case Result.Failure(ps) => assertEquals(ps.map(_.message), NonEmptyChain("Duplicate definition of type 'Episode' found"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: multiple deprecated annotations") {
    val schema = Schema(
      """
        type ExampleType {
          oldField: String @deprecated @deprecated
        }
      """
    )

    schema match {
      case Result.Failure(ps) => assertEquals(ps.map(_.message), NonEmptyChain("Directive 'deprecated' may not occur more than once"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }


  test("schema validation: deprecated annotation with unsupported argument") {
    val schema = Schema(
      """
        type ExampleType {
          oldField: String @deprecated(notareason: "foo bar baz")
        }
      """
    )

    schema match {
      case Result.Failure(ps) => assertEquals(ps.map(_.message), NonEmptyChain("Unknown argument(s) 'notareason' in directive deprecated"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: duplicate enum values") {
    val schema = Schema(
      """
        enum Direction {
          NORTH
          NORTH
        }
      """
    )

    schema match {
      case Result.Failure(ps) => assertEquals(ps.map(_.message), NonEmptyChain("Duplicate definition of enum value 'NORTH' for Enum type 'Direction'"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: object implementing unspecified interfaces") {
    val schema = Schema(
      """
        type Human implements Character & Contactable {
          name: String!
        }
      """
    )

    schema match {
      case Result.Failure(ps) =>
        assertEquals(
          ps.map(_.message),
          NonEmptyChain(
            "Reference to undefined type 'Character'",
            "Reference to undefined type 'Contactable'",
            "Non-interface type 'Character' declared as implemented by type 'Human'",
            "Non-interface type 'Contactable' declared as implemented by type 'Human'"
          )
        )
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: object failing to implement interface fields") {
    val schema = Schema(
      """
        interface Character {
          id: ID!
          name: String!
          email: String!
        }

        type Human implements Character {
          name: String!
        }
      """
    )

    schema match {
      case Result.Failure(ps) =>
        assertEquals(ps.map(_.message), NonEmptyChain("Field 'id' from interface 'Character' is not defined by implementing type 'Human'", "Field 'email' from interface 'Character' is not defined by implementing type 'Human'"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: interface failing to implement interface fields") {
    val schema = Schema(
      """
        interface Character {
          id: ID!
          name: String!
          email: String!
        }

        interface Named implements Character {
          name: String!
        }
      """
    )

    schema match {
      case Result.Failure(ps) =>
        assertEquals(ps.map(_.message), NonEmptyChain("Field 'id' from interface 'Character' is not defined by implementing type 'Named'", "Field 'email' from interface 'Character' is not defined by implementing type 'Named'"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: object implementing interface field with wrong type") {
    val schema = Schema(
      """
        interface Character {
          name: String!
        }

        type Human implements Character {
          name: Int!
        }
      """
    )

    schema match {
      case Result.Failure(ps) =>
        assertEquals(ps.map(_.message), NonEmptyChain("Field 'name' of type 'Human' has type 'Int!', however implemented interface 'Character' requires it to be a subtype of 'String!'"))
      case unexpected => fail(s"This was unexpected: ${unexpected.getClass.getSimpleName}")
    }
  }

  test("schema validation: object implementing interface field with mismatched arguments") {
    val schema = Schema(
      """
        interface Character {
          name(foo: Int!): String!
        }

        type Human implements Character {
          name(foo: String!): String!
        }
      """
    )

    schema match {
      case Result.Failure(ps) =>
        assertEquals(ps.map(_.message), NonEmptyChain("Field 'name' of type 'Human' has has an argument list that does not conform to that specified by implemented interface 'Character'"))
      case unexpected => fail(s"This was unexpected: ${unexpected.getClass.getSimpleName}")
    }
  }

  test("schema validation: multiple objects failing to implement interface field") {
    val schema = Schema(
      """
        interface Character {
          id: ID!
          name: String!
        }

        type Human implements Character {
          name: String!
        }

        type Dog implements Character {
          name: String!
        }
      """
    )

    schema match {
      case Result.Failure(ps) =>
        assertEquals(ps.map(_.message), NonEmptyChain("Field 'id' from interface 'Character' is not defined by implementing type 'Human'", "Field 'id' from interface 'Character' is not defined by implementing type 'Dog'"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: object failing to implement multiple interface fields") {
    val schema = Schema(
      """
        interface Character {
          id: ID!
          name: String!
        }

        interface Contactable {
          email: String!
        }

        type Human implements Character & Contactable {
          name: String!
        }
      """
    )

    schema match {
      case Result.Failure(ps) =>
        assertEquals(ps.map(_.message), NonEmptyChain("Field 'id' from interface 'Character' is not defined by implementing type 'Human'", "Field 'email' from interface 'Contactable' is not defined by implementing type 'Human'"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: object correctly implements transitive interface") {
    val schema = Schema(
      """
        interface Node {
          id: ID!
        }

        interface Resource implements Node {
          id: ID!
          url: String
        }

        type Human implements Resource & Node {
          id: ID!
          url: String
        }
      """
    )

    schema match {
      case Result.Success(a) => assertEquals(a.types.map(_.name), List("Node", "Resource", "Human"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: implements non-interface") {
    val schema = Schema(
      """
        type Query {
          foo: Foo
        }

        type Foo {
          foo: Int
        }

        type Bar implements Foo {
          foo: Int
          bar: String
        }
      """
    )

    schema match {
      case Result.Failure(ps) =>
        assertEquals(ps.map(_.message), NonEmptyChain("Non-interface type 'Foo' declared as implemented by type 'Bar'"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("object extension") {
    val schema = Schema(
      """
        type Query {
          foo: Human
        }

        type Human {
          url: String
        }

        extend type Human {
          id: ID!
        }
      """
    )

    schema match {
      case Result.Success(s) => assertEquals(s.definition("Human").map(_.hasField("id")), Some(true))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: extension on incorrect type") {
    val schema = Schema(
      """
        type Query {
          foo: Episode
        }

        type Episode {
          id: String!
        }

        extend scalar Episode
      """
    )

    schema match {
      case Result.Failure(a) =>
        assertEquals(a.map(_.message), NonEmptyChain("Attempted to apply Scalar extension to Episode but it is not a Scalar"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: extension on non-existent type") {
    val schema = Schema(
      """
        type Query {
          foo: Int
        }

        extend scalar Episode
      """
    )

    schema match {
      case Result.Failure(a) =>
        assertEquals(a.map(_.message), NonEmptyChain("Unable apply extension to non-existent Episode"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("explicit Schema type (complete)") {

    val schema =
      schema"""
        schema {
          query: MyQuery
          mutation: MyMutation
          subscription: MySubscription
        }

        type MyQuery {
          foo: Int
        }

        type MyMutation {
          setFoo(n: Int): Int
        }

        type MySubscription {
          watchFoo: Int
        }
      """

    assert(schema.queryType                 =:= schema.ref("MyQuery"))
    assert(schema.mutationType.exists(_     =:= schema.ref("MyMutation")))
    assert(schema.subscriptionType.exists(_ =:= schema.ref("MySubscription")))

  }

  test("explicit Schema type (partial)") {

    val schema =
      schema"""
        schema {
          query: MyQuery
          mutation: MyMutation
        }

        type MyQuery {
          foo: Int
        }

        type MyMutation {
          setFoo(n: Int): Int
        }
      """

    assert(schema.queryType             =:= schema.ref("MyQuery"))
    assert(schema.mutationType.exists(_ =:= schema.ref("MyMutation")))
    assertEquals(schema.subscriptionType, None)

  }

  test("implicit Schema type") {

    val schema =
      schema"""
        type Query {
          foo: Int
        }

        type Mutation {
          setFoo(n: Int): Int
        }

        type Subscription {
          watchFoo: Int
        }
      """

    assert(schema.queryType                 =:= schema.ref("Query"))
    assert(schema.mutationType.exists(_     =:= schema.ref("Mutation")))
    assert(schema.subscriptionType.exists(_ =:= schema.ref("Subscription")))
  }

  test("no query type (crashes)") {
    intercept[NoSuchElementException](schema"scalar Foo".queryType)
  }

  test("schema validation: fields implementing interfaces can be subtypes") {
    val schema = Schema(
      """
        type Query {
          connections: [Connection!]!
        }
        interface Edge {
          node: String
        }
        interface Connection {
          edges: [Edge!]!
        }
        type MyEdge implements Edge {
          node: String
        }
        type MyConnection implements Connection {
          edges: [MyEdge!]!
        }
      """
    )

    schema match {
      case Result.Success(s) => assertEquals(s.types.map(_.name), List("Query", "Edge", "Connection", "MyEdge", "MyConnection"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: fields with commonly misnamed types") {
    val schema = Schema(
      """
        type Query {
          country(id: Long!): Country
          countries(continent: String): [Country]
        }

        type Country {
          id: Long!
          name: String!
          continent: String!
          bestFood: String
          hasEiffelTower: Boolean!
        }
      """
    )

    schema match {
      case Result.Failure(ps)  =>
        assertEquals(ps.map(_.message), NonEmptyChain("Reference to undefined type 'Long'", "Reference to undefined type 'Long'"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }
}
