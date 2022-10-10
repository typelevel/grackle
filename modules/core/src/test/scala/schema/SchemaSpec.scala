// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package schema

import cats.data.{Ior, NonEmptyChain}
import cats.data.Ior.Both
import cats.tests.CatsSuite
import edu.gemini.grackle.Schema
import edu.gemini.grackle.syntax._

final class SchemaSpec extends CatsSuite {
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
      case Ior.Left(e) => assert(e.head.message == "Reference to undefined type: Episod")
      case Both(a, b)  =>
        assert(a.head.message == "Reference to undefined type: Episod")
        assert(b.types.map(_.name) == List("Query", "Episode"))
      case Ior.Right(b) => fail(s"Shouldn't compile: $b")
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
      case Both(a, b)  =>
        assert(a.head.message == "Reference to undefined type: CCid")
        assert(b.types.map(_.name) == List("Query", "CCId", "Episode"))
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
      case Ior.Left(e) => assert(e.head.message == "Duplicate NamedType found: Episode")
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
      case Ior.Left(e) => assert(e.head.message == "Only a single deprecated allowed at a given location")
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
      case Ior.Left(e) => assert(e.head.message == "deprecated must have a single String 'reason' argument, or no arguments")
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
      case Both(a, b)  =>
        assert(a.head.message == "Duplicate EnumValueDefinition of NORTH for EnumTypeDefinition Direction")
        assert(b.types.map(_.name) == List("Direction"))
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
      case Both(a, b)  =>
        assert(a.map(_.message) == NonEmptyChain("Interface Character implemented by Human is not defined", "Interface Contactable implemented by Human is not defined"))
        assert(b.types.map(_.name) == List("Human"))
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
      case Both(a, b)  =>
        assert(a.map(_.message) == NonEmptyChain("Expected field id from interface Character is not implemented by Human", "Expected field email from interface Character is not implemented by Human"))
        assert(b.types.map(_.name) == List("Character", "Human"))
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
      case Both(a, b)  =>
        assert(a.map(_.message) == NonEmptyChain("Expected field id from interface Character is not implemented by Named", "Expected field email from interface Character is not implemented by Named"))
        assert(b.types.map(_.name) == List("Character", "Named"))
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
      case Both(a, b)  =>
        assert(a.map(_.message) == NonEmptyChain("Field name has type Int!, however implemented interface Character requires it to be of type String!"))
        assert(b.types.map(_.name) == List("Character", "Human"))
      case unexpected => fail(s"This was unexpected: $unexpected")
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
      case Both(a, b)  =>
        assert(a.map(_.message) == NonEmptyChain("Field name of Human has has an argument list that does not match that specified by implemented interface Character"))
        assert(b.types.map(_.name) == List("Character", "Human"))
      case unexpected => fail(s"This was unexpected: $unexpected")
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
      case Both(a, b)  =>
        assert(a.map(_.message) == NonEmptyChain("Expected field id from interface Character is not implemented by Human", "Expected field id from interface Character is not implemented by Dog"))
        assert(b.types.map(_.name) == List("Character", "Human", "Dog"))
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
      case Both(a, b)  =>
        assert(a.map(_.message) == NonEmptyChain("Expected field id from interface Character is not implemented by Human", "Expected field email from interface Contactable is not implemented by Human"))
        assert(b.types.map(_.name) == List("Character", "Contactable", "Human"))
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
      case Ior.Right(a) => assert(a.types.map(_.name) == List("Node", "Resource", "Human"))
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
    assert(schema.subscriptionType       == None)

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

  ignore("no query type (crashes)") {
    schema"scalar Foo".queryType
  }

  test("schema validation: fields implementing interfaces can be subtypes") {
    val schema = Schema("""
      type Query {
        foo: Int
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
    """)

    schema match {
      case Ior.Right(a) => assert(a.types.map(_.name) == List("MyEdge", "MyCinnection"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

}
