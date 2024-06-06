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

package extensions

import cats.data.NonEmptyChain
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._

final class ExtensionsSuite extends CatsEffectSuite {
  test("valid schema extension") {
    val schema =
      schema"""
        schema {
          query: Foo
        }

        extend schema @Sch {
          mutation: Bar
        }

        type Foo {
          foo: String
        }

        type Bar {
          bar: String
        }

        directive @Sch on SCHEMA
      """

    assertEquals(schema.mutationType.map(_.name), Some("Bar"))
    assertEquals(schema.schemaType.directives.map(_.name), List("Sch"))
  }

  test("valid schema extension (no operations)") {
    val schema =
      schema"""
        schema {
          query: Foo
        }

        extend schema @Sch

        type Foo {
          foo: String
        }

        directive @Sch on SCHEMA
      """

    assertEquals(schema.schemaType.directives.map(_.name), List("Sch"))
  }

  test("valid scalar extension") {
    val schema =
      schema"""
        schema {
          query: Foo
        }

        scalar Foo

        extend scalar Foo @Sca

        directive @Sca on SCALAR
      """

    schema.definition("Foo") match {
      case Some(scalar: ScalarType) =>
        assertEquals(scalar.directives.map(_.name), List("Sca"))
      case _ => fail("Foo type not found")
    }
  }

  test("valid object extension") {
    val schema =
      schema"""
        type Query {
          foo: Human
        }

        type Human {
          name: String
        }

        interface Animal {
          species: String
        }

        extend type Human implements Animal @Obj {
          id: ID!
          species: String
        }

        directive @Obj on OBJECT
      """

    schema.definition("Human") match {
      case Some(obj: ObjectType) =>
        assertEquals(obj.fields.map(_.name), List("name", "id", "species"))
        assertEquals(obj.interfaces.map(_.name), List("Animal"))
        assertEquals(obj.directives.map(_.name), List("Obj"))
      case _ => fail("Human type not found")
    }
  }

  test("valid object extension (no fields)") {
    val schema =
      schema"""
        type Query {
          foo: Human
        }

        type Human {
          name: String
        }

        interface Animal {
          name: String
        }

        extend type Human implements Animal @Obj

        directive @Obj on OBJECT
      """

    schema.definition("Human") match {
      case Some(obj: ObjectType) =>
        assertEquals(obj.interfaces.map(_.name), List("Animal"))
        assertEquals(obj.directives.map(_.name), List("Obj"))
      case _ => fail("Human type not found")
    }
  }

  test("valid interface extension") {
    val schema =
      schema"""
        type Query {
          foo: Human
        }

        type Human implements Animal {
          name: String
          species: String
        }

        interface Animal {
          species: String
        }

        interface Organism {
          extinct: Boolean
        }

        extend interface Animal implements Organism @Intrf {
          id: ID!
          extinct: Boolean
        }

        extend type Human implements Organism {
          id: ID!
          extinct: Boolean
        }

        directive @Intrf on INTERFACE
      """

    schema.definition("Animal") match {
      case Some(intrf: InterfaceType) =>
        assertEquals(intrf.fields.map(_.name), List("species", "id", "extinct"))
        assertEquals(intrf.interfaces.map(_.name), List("Organism"))
        assertEquals(intrf.directives.map(_.name), List("Intrf"))
      case _ => fail("Animal type not found")
    }

    schema.definition("Human") match {
      case Some(obj: ObjectType) =>
        assertEquals(obj.fields.map(_.name), List("name", "species", "id", "extinct"))
        assertEquals(obj.interfaces.map(_.name), List("Animal", "Organism"))
      case _ => fail("Human type not found")
    }
  }

  test("valid interface extension (no fields)") {
    val schema =
      schema"""
        type Query {
          foo: Human
        }

        type Human implements Animal {
          name: String
          species: String
        }

        interface Animal {
          species: String
        }

        interface Organism {
          species: String
        }

        extend interface Animal implements Organism @Intrf

        extend type Human implements Organism

        directive @Intrf on INTERFACE
      """

    schema.definition("Animal") match {
      case Some(intrf: InterfaceType) =>
        assertEquals(intrf.interfaces.map(_.name), List("Organism"))
        assertEquals(intrf.directives.map(_.name), List("Intrf"))
      case _ => fail("Animal type not found")
    }
  }

  test("valid union extension") {
    val schema =
      schema"""
        type Query {
          foo: Human
        }

        type Human {
          name: String
        }

        type Dog {
          name: String
        }

        type Cat {
          name: String
        }

        union Animal = Human | Dog

        extend union Animal @Uni = Cat

        directive @Uni on UNION
      """

    schema.definition("Animal") match {
      case Some(u: UnionType) =>
        assertEquals(u.members.map(_.name), List("Human", "Dog", "Cat"))
        assertEquals(u.directives.map(_.name), List("Uni"))
      case _ => fail("Animal type not found")
    }
  }

  test("valid union extension (no members)") {
    val schema =
      schema"""
        type Query {
          foo: Human
        }

        type Human {
          name: String
        }

        type Dog {
          name: String
        }

        union Animal = Human | Dog

        extend union Animal @Uni

        directive @Uni on UNION
      """

    schema.definition("Animal") match {
      case Some(u: UnionType) =>
        assertEquals(u.directives.map(_.name), List("Uni"))
      case _ => fail("Animal type not found")
    }
  }

  test("valid enum extension") {
    val schema =
      schema"""
        type Query {
          foo: Animal
        }

        enum Animal { Human, Dog }

        extend enum Animal @Enu { Cat }

        directive @Enu on ENUM
      """

    schema.definition("Animal") match {
      case Some(e: EnumType) =>
        assertEquals(e.enumValues.map(_.name), List("Human", "Dog", "Cat"))
        assertEquals(e.directives.map(_.name), List("Enu"))
      case _ => fail("Animal type not found")
    }
  }

  test("valid enum extension (no values)") {
    val schema =
      schema"""
        type Query {
          foo: Animal
        }

        enum Animal { Human, Dog }

        extend enum Animal @Enu

        directive @Enu on ENUM
      """

    schema.definition("Animal") match {
      case Some(e: EnumType) =>
        assertEquals(e.directives.map(_.name), List("Enu"))
      case _ => fail("Animal type not found")
    }
  }

  test("valid input object extension") {
    val schema =
      schema"""
        type Query {
          foo(arg: Animal): Int
        }

        input Animal {
          name: String
        }

        extend input Animal @Inp {
          species: String
        }

        directive @Inp on INPUT_OBJECT
      """

    schema.definition("Animal") match {
      case Some(inp: InputObjectType) =>
        assertEquals(inp.inputFields.map(_.name), List("name", "species"))
        assertEquals(inp.directives.map(_.name), List("Inp"))
      case _ => fail("Animal type not found")
    }
  }

  test("valid input object extension (no fields)") {
    val schema =
      schema"""
        type Query {
          foo(arg: Animal): Int
        }

        input Animal {
          name: String
        }

        extend input Animal @Inp

        directive @Inp on INPUT_OBJECT
      """

    schema.definition("Animal") match {
      case Some(inp: InputObjectType) =>
        assertEquals(inp.directives.map(_.name), List("Inp"))
      case _ => fail("Animal type not found")
    }
  }

  test("invalid extension on incorrect type") {
    val schema = Schema(
      """
        type Query {
          foo: Scalar
        }

        scalar Scalar
        type Object {
          id: String!
        }
        interface Interface {
          id: String!
        }
        union Union = Object
        enum Enum { A, B, C }
        input Input { id: String! }

        extend type Scalar @Sca
        extend interface Object @Obj
        extend union Interface @Intrf
        extend enum Union @Uni
        extend input Enum @Enu
        extend scalar Input @Inp

        directive @Sca on SCALAR
        directive @Obj on OBJECT
        directive @Intrf on INTERFACE
        directive @Uni on UNION
        directive @Enu on ENUM
        directive @Inp on INPUT_OBJECT
      """
    )

    val expected =
      NonEmptyChain(
        "Attempted to apply Object extension to Scalar but it is not a Object",
        "Attempted to apply Interface extension to Object but it is not a Interface",
        "Attempted to apply Union extension to Interface but it is not a Union",
        "Attempted to apply Enum extension to Union but it is not a Enum",
        "Attempted to apply Input Object extension to Enum but it is not a Input Object",
        "Attempted to apply Scalar extension to Input but it is not a Scalar"
      )

    schema match {
      case Result.Failure(a) =>
        assertEquals(a.map(_.message), expected)
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("invalid extension of non-existent type") {
    val schema = Schema(
      """
        type Query {
          foo: Int
        }

        extend scalar Scalar
        extend interface Interface
        extend type Object
        extend union Union
        extend enum Enum
        extend input Input
      """
    )

    val expected =
      NonEmptyChain(
        "Unable apply extension to non-existent Scalar",
        "Unable apply extension to non-existent Interface",
        "Unable apply extension to non-existent Object",
        "Unable apply extension to non-existent Union",
        "Unable apply extension to non-existent Enum",
        "Unable apply extension to non-existent Input"
      )

    schema match {
      case Result.Failure(a) =>
        assertEquals(a.map(_.message), expected)
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("invalid schema extension") {
    val schema = Schema(
      """
        schema {
          query: Foo
        }

        extend schema @Sca

        type Foo {
          foo: String
        }

        directive @Sca on SCALAR
      """
    )

    val expected =
      NonEmptyChain(
        "Directive 'Sca' is not allowed on SCHEMA"
      )

    schema match {
      case Result.Failure(a) =>
        assertEquals(a.map(_.message), expected)
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("invalid scalar extension") {
    val schema = Schema(
      """
        schema {
          query: Foo
        }

        scalar Foo

        extend scalar Foo @Obj

        directive @Obj on OBJECT
      """
    )

    val expected =
      NonEmptyChain(
        "Directive 'Obj' is not allowed on SCALAR"
      )

    schema match {
      case Result.Failure(a) =>
        assertEquals(a.map(_.message), expected)
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("invalid object extension") {
    val schema = Schema(
      """
        type Query {
          foo: Human
        }

        type Human {
          name: String
        }

        interface Animal {
          species: String
        }

        extend type Human implements Animal & Organism @Sca {
          name: String
        }

        directive @Sca on SCALAR
      """
    )

    val expected =
      NonEmptyChain(
        "Duplicate definition of field 'name' for type 'Human'",
        "Field 'species' from interface 'Animal' is not defined by implementing type 'Human'",
        "Undefined type 'Organism' declared as implemented by type 'Human'",
        "Directive 'Sca' is not allowed on OBJECT"
      )

    schema match {
      case Result.Failure(a) =>
        assertEquals(a.map(_.message), expected)
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("invalid interface extension") {
    val schema = Schema(
      """
        type Query {
          foo: Human
        }

        type Human implements Animal {
          name: String
          species: String
        }

        interface Animal {
          species: String
        }

        interface Organism {
          extinct: Boolean
        }

        extend interface Animal implements Organism & Mineral @Sca {
          species: String
        }

        directive @Sca on SCALAR
      """
    )

    val expected =
      NonEmptyChain(
        "Duplicate definition of field 'species' for type 'Animal'",
        "Type 'Human' does not directly implement transitively implemented interfaces: 'Organism', 'Mineral'",
        "Field 'extinct' from interface 'Organism' is not defined by implementing type 'Animal'",
        "Undefined type 'Mineral' declared as implemented by type 'Animal'",
        "Directive 'Sca' is not allowed on INTERFACE"
      )

    schema match {
      case Result.Failure(a) =>
        assertEquals(a.map(_.message), expected)
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("invalid union extension") {
    val schema = Schema(
      """
        type Query {
          foo: Human
        }

        type Human {
          name: String
        }

        type Dog {
          name: String
        }

        union Animal = Human | Dog

        extend union Animal @Sca = Dog | Cat | Int

        directive @Sca on SCALAR
      """
    )

    val expected =
      NonEmptyChain(
        "Duplicate inclusion of union member 'Dog' for type 'Animal'",
        "Undefined type 'Cat' included in union 'Animal'",
        "Non-object type 'Int' included in union 'Animal'",
        "Directive 'Sca' is not allowed on UNION"
      )

    schema match {
      case Result.Failure(a) =>
        assertEquals(a.map(_.message), expected)
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("invalid enum extension") {
    val schema = Schema(
      """
        type Query {
          foo: Animal
        }

        enum Animal { Human, Dog }

        extend enum Animal @Sca { Dog }

        directive @Sca on SCALAR
      """
    )

    val expected =
      NonEmptyChain(
        "Duplicate definition of enum value 'Dog' for type 'Animal'",
        "Directive 'Sca' is not allowed on ENUM"
      )

    schema match {
      case Result.Failure(a) =>
        assertEquals(a.map(_.message), expected)
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("invalid input object extension") {
    val schema = Schema(
      """
        type Query {
          foo(arg: Animal): Int
        }

        input Animal {
          name: String
        }

        extend input Animal @Sca {
          name: String
        }

        directive @Sca on SCALAR
      """
    )

    val expected =
      NonEmptyChain(
        "Duplicate definition of field 'name' for type 'Animal'",
        "Directive 'Sca' is not allowed on INPUT_OBJECT"
      )

    schema match {
      case Result.Failure(a) =>
        assertEquals(a.map(_.message), expected)
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }
}
