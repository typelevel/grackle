// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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

package sdl

import munit.CatsEffectSuite

import grackle.{GraphQLParser, SchemaParser, SchemaRenderer}
import grackle.Ast._
import grackle.Ast.OperationType._
import grackle.Ast.Type.{List => _, _}
import grackle.syntax._

final class SDLSuite extends CatsEffectSuite {
  val parser = GraphQLParser(GraphQLParser.defaultConfig)
  val schemaParser = SchemaParser(parser)

  val TQ = "\"" * 3

  test("parse schema definition") {
    val schema = """
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
            RootOperationTypeDefinition(Query, Named(Name("MyQuery")), Nil),
            RootOperationTypeDefinition(Mutation, Named(Name("MyMutation")), Nil),
            RootOperationTypeDefinition(Subscription, Named(Name("MySubscription")), Nil)
          ),
          Nil
        )
      )

    val res = parser.parseText(schema)

    assertEquals(res, expected.success)
  }

  test("parse scalar type definition") {
    val schema = """
      "A scalar type"
      scalar Url

      "A scalar type"
      scalar Time @deprecated
    """

    val expected =
      List(
        ScalarTypeDefinition(Name("Url"), Some("A scalar type"), Nil),
        ScalarTypeDefinition(
          Name("Time"),
          Some("A scalar type"),
          List(Directive(Name("deprecated"), Nil)))
      )

    val res = parser.parseText(schema)

    assertEquals(res, expected.success)
  }

  test("parse object type definition") {
    val schema = """
      "An object type"
      type Query {
        posts: [Post]
        author(id: Int!): Author
      }
    """

    val expected =
      List(
        ObjectTypeDefinition(
          Name("Query"),
          Some("An object type"),
          List(
            FieldDefinition(Name("posts"), None, Nil, Type.List(Named(Name("Post"))), Nil),
            FieldDefinition(
              Name("author"),
              None,
              List(
                InputValueDefinition(
                  Name("id"),
                  None,
                  NonNull(Left(Named(Name("Int")))),
                  None,
                  Nil)),
              Named(Name("Author")),
              Nil
            )
          ),
          Nil,
          Nil
        )
      )

    val res = parser.parseText(schema)

    assertEquals(res, expected.success)
  }

  test("parse interface type definition") {
    val schema = """
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
        InterfaceTypeDefinition(
          Name("Post"),
          Some("An interface type"),
          List(
            FieldDefinition(
              Name("id"),
              Some("A field"),
              Nil,
              NonNull(Left(Named(Name("Int")))),
              Nil),
            FieldDefinition(Name("title"), None, Nil, Named(Name("String")), Nil),
            FieldDefinition(
              Name("author"),
              Some("A deprecated field"),
              Nil,
              Named(Name("Author")),
              List(Directive(Name("deprecated"), Nil))),
            FieldDefinition(Name("votes"), None, Nil, Named(Name("Int")), Nil)
          ),
          Nil,
          Nil
        )
      )

    val res = parser.parseText(schema)

    assertEquals(res, expected.success)
  }

  test("parse union type definition") {
    val schema = """
      "A union type"
      union ThisOrThat = This | That
    """

    val expected =
      List(
        UnionTypeDefinition(
          Name("ThisOrThat"),
          Some("A union type"),
          Nil,
          List(
            Named(Name("This")),
            Named(Name("That"))
          )
        )
      )

    val res = parser.parseText(schema)

    assertEquals(res, expected.success)
  }

  test("parse enum type definition") {
    val schema = """
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
        EnumTypeDefinition(
          Name("Direction"),
          Some("An enum type"),
          Nil,
          List(
            EnumValueDefinition(Name("NORTH"), None, Nil),
            EnumValueDefinition(Name("EAST"), None, Nil),
            EnumValueDefinition(Name("SOUTH"), None, Nil),
            EnumValueDefinition(Name("WEST"), None, Nil)
          )
        )
      )

    val res = parser.parseText(schema)

    assertEquals(res, expected.success)
  }

  test("parse input object type definition") {
    val schema = """
      "An input object type"
      input Point2D {
        x: Float
        y: Float
      }
    """

    val expected =
      List(
        InputObjectTypeDefinition(
          Name("Point2D"),
          Some("An input object type"),
          List(
            InputValueDefinition(Name("x"), None, Named(Name("Float")), None, Nil),
            InputValueDefinition(Name("y"), None, Named(Name("Float")), None, Nil)
          ),
          Nil
        )
      )

    val res = parser.parseText(schema)

    assertEquals(res, expected.success)
  }

  test("parse directive definition") {
    val schema =
      """|type Query {
         |  foo: Int
         |}
         |"A directive"
         |directive @delegateField(name: String!) repeatable on OBJECT|INTERFACE|FIELD|FIELD_DEFINITION|ENUM|ENUM_VALUE
         |""".stripMargin

    val res = schemaParser.parseText(schema)
    val ser = res.map(_.toString)

    assertEquals(ser, schema.success)
  }

  test("deserialize schema (1)") {
    val schema =
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

    val res = schemaParser.parseText(schema)
    val ser = res.map(_.toString)

    assertEquals(ser, schema.success)
  }

  test("deserialize schema (2)") {
    val schema =
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
         |  fullname: String @deprecated(reason: "use 'name' instead")
         |  friends: [Character!]
         |  appearsIn: [Episode!]
         |  homePlanet: String
         |}
         |type Droid implements Character {
         |  id: String!
         |  name: String
         |  fullname: String @deprecated(reason: "use 'name' instead")
         |  friends: [Character!]
         |  appearsIn: [Episode!]
         |  primaryFunction: String
         |}""".stripMargin

    val res = schemaParser.parseText(schema)
    val ser = res.map(_.toString)

    assertEquals(ser, schema.success)
  }

  test("deserialize schema (3)") {
    val schema =
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

    val res = schemaParser.parseText(schema)
    val ser = res.map(_.toString)

    assertEquals(ser, schema.success)
  }

  test("round-trip extensions") {
    val schema =
      """|schema {
         |  query: MyQuery
         |}
         |type MyQuery {
         |  foo(s: Scalar, i: Input, e: Enum): Union
         |}
         |type Mutation {
         |  bar: Int
         |}
         |scalar Scalar
         |interface Intrf {
         |  bar: String
         |}
         |type Obj1 implements Intrf {
         |  bar: String
         |}
         |type Obj2 implements Intrf {
         |  bar: String
         |}
         |union Union = Obj1 | Obj2
         |enum Enum {
         |  A
         |  B
         |}
         |input Input {
         |  baz: Float
         |}
         |type Quux {
         |  quux: String
         |}
         |extend schema @Sch {
         |  mutation: Mutation
         |}
         |extend scalar Scalar @Sca
         |extend interface Intrf @Intrf {
         |  baz: Boolean
         |}
         |extend type Obj1 @Obj {
         |  baz: Boolean
         |  quux: String
         |}
         |extend type Obj2 @Obj {
         |  baz: Boolean
         |  quux: String
         |}
         |extend union Union @Uni = Quux
         |extend enum Enum @Enu {
         |  C
         |}
         |extend input Input @Inp {
         |  foo: Int
         |}
         |directive @Sch on SCHEMA
         |directive @Sca on SCALAR
         |directive @Obj on OBJECT
         |directive @Intrf on INTERFACE
         |directive @Uni on UNION
         |directive @Enu on ENUM
         |directive @Inp on INPUT_OBJECT
         |""".stripMargin

    val res = schemaParser.parseText(schema)
    val ser = res.map(_.toString)

    assertEquals(ser, schema.success)
  }

  test("round-trip extensions (no fields or members)") {
    val schema =
      """|schema {
         |  query: MyQuery
         |}
         |type MyQuery {
         |  foo(s: Scalar, i: Input, e: Enum): Union
         |}
         |scalar Scalar
         |interface Intrf {
         |  bar: String
         |}
         |type Obj1 implements Intrf {
         |  bar: String
         |}
         |type Obj2 implements Intrf {
         |  bar: String
         |}
         |union Union = Obj1 | Obj2
         |enum Enum {
         |  A
         |  B
         |}
         |input Input {
         |  baz: Float
         |}
         |extend schema @Sch
         |extend scalar Scalar @Sca
         |extend interface Intrf @Intrf
         |extend type Obj1 @Obj
         |extend type Obj2 @Obj
         |extend union Union @Uni
         |extend enum Enum @Enu
         |extend input Input @Inp
         |directive @Sch on SCHEMA
         |directive @Sca on SCALAR
         |directive @Obj on OBJECT
         |directive @Intrf on INTERFACE
         |directive @Uni on UNION
         |directive @Enu on ENUM
         |directive @Inp on INPUT_OBJECT
         |""".stripMargin

    val res = schemaParser.parseText(schema)
    val ser = res.map(_.toString)

    assertEquals(ser, schema.success)
  }

  test("deserialize schema with descriptions") {
    val schema =
      """|"A scalar"
         |scalar Scalar
         |"An interface"
         |interface Intrf {
         |  "An interface field"
         |  foo: Int
         |}
         |"An object"
         |type Obj implements Intrf {
         |  "An object field"
         |  foo: Int
         |  bar(
         |    "An argument"
         |    baz: Int
         |  ): String
         |}
         |"A union"
         |union Union = Obj
         |"An enum"
         |enum Enum {
         |  "An enum value"
         |  A
         |  B
         |}
         |"An input object"
         |input Input {
         |  "An input field"
         |  baz: Float
         |}
         |type Query {
         |  obj: Obj
         |}""".stripMargin

    val res = schemaParser.parseText(schema)
    val ser = res.map(_.toString)

    assertEquals(ser, schema.success)
  }

  test("deserialize schema with described directive arguments") {
    val schema =
      """|type Query {
         |  foo: Int
         |}
         |"A directive"
         |directive @dir(
         |  "An argument"
         |  baz: Int
         |) on FIELD
         |""".stripMargin

    val res = schemaParser.parseText(schema)
    val ser = res.map(_.toString)

    assertEquals(ser, schema.success)
  }

  test("deserialize schema with block descriptions") {
    val schema =
      s"""|$TQ
          |A scalar described
          |over several lines.
          |$TQ
          |scalar Scalar
          |type Query {
          |  $TQ
          |  A field with a "quoted" word,
          |  a backslash \\w, and more.
          |  $TQ
          |  foo: Scalar
          |}""".stripMargin

    val res = schemaParser.parseText(schema)
    val ser = res.map(_.toString)

    assertEquals(ser, schema.success)
  }

  test("descriptions survive a parse/render/parse round trip") {
    val schema =
      s"""|type Query {
          |  $TQ
          |  Indented | pipes, "quotes" and a trailing backslash \\
          |  $TQ
          |  foo: Int
          |}
          |""".stripMargin

    val once = schemaParser.parseText(schema).map(_.toString)
    val twice = once.flatMap(schemaParser.parseText).map(_.toString)

    assertEquals(twice, once)
  }

  /**
   * Descriptions which exercise the corners of both the quoted and the block string forms.
   */
  val trickyDescriptions: List[(String, String)] =
    List(
      "quotes" -> "he said \"hi\"",
      "triple quotes" -> s"a ${TQ} b",
      "four quotes" -> s"a ${TQ}\" b",
      "trailing quote" -> "a trailing quote \"",
      "trailing backslash" -> "a backslash \\",
      "tab" -> "first\tsecond"
    )

  trickyDescriptions.foreach {
    case (label, desc) =>
      test(s"description round trips: $label") {
        val rendered = SchemaRenderer.renderDescription(Some(desc))

        val topLevel =
          s"""|${rendered}scalar Scalar
              |type Query {
              |  foo: Scalar
              |}""".stripMargin

        assertEquals(
          schemaParser.parseText(topLevel).map(_.definition("Scalar").flatMap(_.description)),
          Some(desc).success,
          clue = s"top level, rendered as: ${escape(rendered)}"
        )

        val indented =
          rendered.linesIterator.map(l => if (l.isEmpty) l else s"  $l").mkString("\n")
        val nested =
          s"""|type Query {
              |$indented
              |  foo: Int
              |}""".stripMargin

        assertEquals(
          schemaParser
            .parseText(nested)
            .map(
              _.definition("Query")
                .collect { case o: grackle.ObjectType => o }
                .flatMap(_.fields.find(_.name == "foo"))
                .flatMap(_.description)),
          Some(desc).success,
          clue = s"nested, rendered as: ${escape(rendered)}"
        )
      }
  }

  /**
   * Renders control characters visibly, so that failure clues are legible.
   */
  private def escape(s: String): String =
    s.flatMap(c => if (c.isControl) f"\\u${c.toInt}%04x" else c.toString)
}
