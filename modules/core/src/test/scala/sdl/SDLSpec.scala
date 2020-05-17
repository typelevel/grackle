// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package sdl

import atto.Atto._
import cats.tests.CatsSuite

import edu.gemini.grackle.{ Ast, GraphQLParser, SchemaParser }
import Ast._, OperationType._, Type.{ List => _, _ }

final class SDLSuite extends CatsSuite {
  test("parse schema definition") {
    val text = """
    schema {
      query: MyQuery
      mutation: MyMutation
      subscription: MySubscription
    }
    """

    val expected =
      List(
        SchemaDefinition(
          List(
            RootOperationTypeDefinition(Query, Named(Name("MyQuery"))),
            RootOperationTypeDefinition(Mutation, Named(Name("MyMutation"))),
            RootOperationTypeDefinition(Subscription, Named(Name("MySubscription")))
          ),
          Nil
        )
      )

    val res = GraphQLParser.Document.parseOnly(text).option
    //println(res)
    assert(res == Some(expected))
  }

  test("parse scalar type definition") {
    val text = """
      "A scalar type"
      scalar Url

      "A scalar type"
      scalar Time @deprecated
    """

    val expected =
      List(
        ScalarTypeDefinition(Name("Url"), Some("A scalar type"), Nil),
        ScalarTypeDefinition(Name("Time"), Some("A scalar type"), List(Directive(Name("deprecated"), Nil)))
      )

    val res = GraphQLParser.Document.parseOnly(text).option
    //println(res)
    assert(res == Some(expected))
  }

  test("parse object type definition") {
    val text = """
      "An object type"
      type Query {
        posts: [Post]
        author(id: Int!): Author
      }
    """

    val expected =
      List(
        ObjectTypeDefinition(Name("Query"), Some("An object type"),
          List(
            FieldDefinition(Name("posts"), None, Nil, Type.List(Named(Name("Post"))), Nil),
            FieldDefinition(
              Name("author"),
              None,
              List(InputValueDefinition(Name("id"), None, NonNull(Left(Named(Name("Int")))), None, Nil)),
              Named(Name("Author")),
              Nil
            )
          ),
          Nil,
          Nil
        )
      )

    val res = GraphQLParser.Document.parseOnly(text).option
    //println(res)
    assert(res == Some(expected))
  }

  test("parse interface type definition") {
    val text = """
      "An interface type"
      interface Post {
        "A field"
        id: Int!
        title: String
        "A deprecated field"
        author: Author @deprecated
        votes: Int
      }
    """

    val expected =
      List(
        InterfaceTypeDefinition(Name("Post"), Some("An interface type"),
          List(
            FieldDefinition(Name("id"), Some("A field"), Nil, NonNull(Left(Named(Name("Int")))), Nil),
            FieldDefinition(Name("title"), None, Nil, Named(Name("String")), Nil),
            FieldDefinition(Name("author"), Some("A deprecated field"), Nil, Named(Name("Author")), List(Directive(Name("deprecated"), Nil))),
            FieldDefinition(Name("votes"), None, Nil, Named(Name("Int")), Nil)
          ),
          Nil,
          Nil
        )
      )

    val res = GraphQLParser.Document.parseOnly(text).option
    //println(res)
    assert(res == Some(expected))
  }

  test("parse union type definition") {
    val text = """
      "A union type"
      union ThisOrThat = This | That
    """

    val expected =
      List(
        UnionTypeDefinition(Name("ThisOrThat"), Some("A union type"), Nil,
          List(
            Named(Name("This")),
            Named(Name("That"))
          )
        )
      )

    val res = GraphQLParser.Document.parseOnly(text).option
    //println(res)
    assert(res == Some(expected))
  }

  test("parse enum type definition") {
    val text = """
      "An enum type"
      enum Direction {
        NORTH
        EAST
        SOUTH
        WEST
      }
    """

    val expected =
      List(
        EnumTypeDefinition(Name("Direction"), Some("An enum type"), Nil,
          List(
            EnumValueDefinition(Name("NORTH"), None, Nil),
            EnumValueDefinition(Name("EAST"), None, Nil),
            EnumValueDefinition(Name("SOUTH"), None, Nil),
            EnumValueDefinition(Name("WEST"), None, Nil)
          )
        )
      )

    val res = GraphQLParser.Document.parseOnly(text).option
    //println(res)
    assert(res == Some(expected))
  }

  test("parse input object type definition") {
    val text = """
      "An input object type"
      input Point2D {
        x: Float
        y: Float
      }
    """

    val expected =
      List(
        InputObjectTypeDefinition(Name("Point2D"), Some("An input object type"),
          List(
            InputValueDefinition(Name("x"), None, Named(Name("Float")), None, Nil),
            InputValueDefinition(Name("y"), None, Named(Name("Float")), None, Nil)
          ),
          Nil
        )
      )

    val res = GraphQLParser.Document.parseOnly(text).option
    //println(res)
    assert(res == Some(expected))
  }

  test("parse directive definition") {
    val text = """
      "A directive"
      directive @delegateField(name: String!) repeatable on OBJECT | INTERFACE
    """

    val expected =
      List(
        DirectiveDefinition(
          Name("delegateField"),
          Some("A directive"),
          List(
            InputValueDefinition(Name("name"), None, NonNull(Left(Named(Name("String")))), None, Nil)
          ),
          true,
          List(DirectiveLocation.OBJECT, DirectiveLocation.INTERFACE)
        )
      )

    val res = GraphQLParser.Document.parseOnly(text).option
    //println(res)
    assert(res == Some(expected))
  }

  test("deserialize schema (1)") {
    val text =
    """|type Author {
       |  id: Int!
       |  firstName: String
       |  lastName: String
       |  posts: [Post]
       |}
       |type Post {
       |  id: Int!
       |  title: String
       |  author: Author
       |  votes: Int
       |}
       |type Query {
       |  posts: [Post]
       |  author(id: Int! = 23): Author
       |}""".stripMargin

    val res = SchemaParser.parseText(text)
    val ser = res.map(_.toString)
    //println(ser.right.get)
    assert(ser.right.get == text)
  }

  test("deserialize schema (2)") {
    val text =
    """|type Query {
       |  hero(episode: Episode!): Character!
       |  character(id: ID!): Character
       |  human(id: ID!): Human
       |  droid(id: ID!): Droid
       |}
       |enum Episode {
       |  ROGUEONE @deprecated(reason: "use NEWHOPE instead")
       |  NEWHOPE
       |  EMPIRE
       |  JEDI
       |}
       |interface Character {
       |  id: String!
       |  name: String
       |  fullname: String @deprecated(reason: "use 'name' instead")
       |  friends: [Character!]
       |  appearsIn: [Episode!]
       |}
       |type Human implements Character {
       |  id: String!
       |  name: String
       |  friends: [Character!]
       |  appearsIn: [Episode!]
       |  homePlanet: String
       |}
       |type Droid implements Character {
       |  id: String!
       |  name: String
       |  friends: [Character!]
       |  appearsIn: [Episode!]
       |  primaryFunction: String
       |}""".stripMargin

    val res = SchemaParser.parseText(text)
    val ser = res.map(_.toString)
    //println(ser.right.get)
    assert(ser.right.get == text)
  }

  test("deserialize schema (3)") {
    val text =
    """|type Query {
       |  whatsat(p: Point!): ThisOrThat!
       |}
       |union ThisOrThat = This | That
       |type This {
       |  id: ID!
       |}
       |type That {
       |  id: ID!
       |}
       |input Point {
       |  x: Int
       |  y: Int
       |}""".stripMargin

    val res = SchemaParser.parseText(text)
    val ser = res.map(_.toString)
    //println(ser.right.get)
    assert(ser.right.get == text)
  }
}
