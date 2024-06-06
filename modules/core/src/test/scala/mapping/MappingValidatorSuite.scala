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

package validator

import cats.syntax.all._
import munit.CatsEffectSuite

import grackle.{Path, ValidationException}
import grackle.syntax._

import compiler.TestMapping

final class ValidatorSuite extends CatsEffectSuite {
  test("missing type mapping") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo: Foo
          }

          type Foo {
            bar: String
          }
        """

      override val typeMappings =
        TypeMappings.unchecked(
          List(
            ObjectMapping(
              schema.ref("Query"),
              List(
                CursorField[String]("foo", _ => ???, Nil)
              )
            )
          )
        )
    }

    val es = M.validate()
    es match {
      case List(M.MissingTypeMapping(_)) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("ambiguous type mapping") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo: Foo
          }

          type Foo {
            bar: String
          }
        """

      override val typeMappings =
        TypeMappings.unchecked(
          ObjectMapping(schema.ref("Query"))(
            CursorField[String]("foo", _ => ???, Nil)
          ),
          ObjectMapping(schema.ref("Foo"))(
            CursorField[String]("bar", _ => ???, Nil)
          ),
          ObjectMapping(MappingPredicate.PathMatch(Path.from(schema.ref("Foo"))))(
            CursorField[String]("bar", _ => ???, Nil)
          )
        )
    }

    val es = M.validate()
    es match {
      case List(M.AmbiguousTypeMappings(_, _)) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("missing field mapping") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo: Foo
          }

          type Foo {
            bar: String
          }
        """

      override val typeMappings =
        TypeMappings.unchecked(
          List(
            ObjectMapping(
              schema.ref("Query"),
              List(
                CursorField[String]("foo", _ => ???, Nil)
              )
            ),
            ObjectMapping(
              schema.ref("Foo"),
              Nil
            )
          )
        )
    }

    val es = M.validate()
    es match {
      case List(M.MissingFieldMapping(_, f)) if f.name == "bar" => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("inapplicable type (object mapping for scalar)") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo: Foo
          }

          scalar Foo
        """

      override val typeMappings =
        TypeMappings.unchecked(
          List(
            ObjectMapping(
              schema.ref("Query"),
              List(
                CursorField[String]("foo", _ => ???, Nil)
              )
            ),
            ObjectMapping(
              schema.ref("Foo"),
              Nil
            )
          )
        )
    }

    val es = M.validate()
    es match {
      case List(M.ObjectTypeExpected(_)) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("inapplicable type (leaf mapping for object)") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo: Foo
          }

          type Foo {
            bar: String
          }
        """

      override val typeMappings =
        TypeMappings.unchecked(
          List(
            ObjectMapping(
              schema.ref("Query"),
              List(
                CursorField[String]("foo", _ => ???, Nil)
              )
            ),
            LeafMapping[String](schema.ref("Foo"))
          )
        )
    }

    val es = M.validate()
    es match {
      case List(M.LeafTypeExpected(_)) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("enums are valid leaf mappings") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo: Foo
          }

          enum Foo { BAR }
        """

      override val typeMappings =
        List(
          ObjectMapping(
            schema.ref("Query"),
            List(
              CursorField[String]("foo", _ => ???, Nil)
            )
          ),
          LeafMapping[String](schema.ref("Foo"))
        )
    }

    val es = M.validate()
    es match {
      case Nil => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("lists are valid leaf mappings") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            baz: Baz
          }

          enum Foo { BAR }

          type Baz {
            quux: [Foo]
          }
        """

      override val typeMappings =
        List(
          ObjectMapping(
            schema.ref("Query"),
            List(
              CursorField[String]("baz", _ => ???, Nil)
            )
          ),
        ObjectMapping(
          schema.ref("Baz"),
          List(
            CursorField[String]("quux", _ => ???, Nil)
          )
        ),
        LeafMapping[String](schema.ref("Foo"))
      )
    }

    val es = M.validate()
    es match {
      case Nil => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("input object types don't require mappings") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo(in: Foo): Int
          }

          input Foo { bar: String }
        """

      override val typeMappings =
        List(
          ObjectMapping(
            schema.ref("Query"),
            List(
              CursorField[String]("foo", _ => ???, Nil)
            )
          )
        )
    }

    val es = M.validate()
    es match {
      case Nil => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("input only enums are valid primitive mappings") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo(in: Foo): Int
          }

          input Foo { bar: Bar }

          enum Bar { BAZ }
        """

      override val typeMappings =
        List(
          ObjectMapping(
            schema.ref("Query"),
            List(
              CursorField[String]("foo", _ => ???, Nil)
            )
          )
        )
    }

    val es = M.validate()
    es match {
      case Nil => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }


  test("nonexistent type (type mapping)") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo: Int
          }
        """

      override val typeMappings =
        TypeMappings.unchecked(
          List(
            ObjectMapping(
              schema.ref("Query"),
              List(
                CursorField[String]("foo", _ => ???, Nil)
              )
            ),
            ObjectMapping(
              schema.uncheckedRef("Foo"),
              Nil
            )
          )
        )
    }

    val es = M.validate()
    es match {
      case List(M.ReferencedTypeDoesNotExist(_)) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("unknown field") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo: Foo
          }

          type Foo {
            bar: String
          }
        """

      override val typeMappings =
        TypeMappings.unchecked(
          List(
            ObjectMapping(
              schema.ref("Query"),
              List(
                CursorField[String]("foo", _ => ???, Nil)
              )
            ),
            ObjectMapping(
              schema.ref("Foo"),
              List(
                CursorField[String]("bar", _ => ???, Nil),
                CursorField[String]("quz", _ => ???, Nil),
              )
            )
          )
        )
    }

    val es = M.validate()
    es match {
      case List(M.UnusedFieldMapping(_, _)) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }
  }

  test("non-field attributes are valid") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo: Foo
          }

          type Foo {
            bar: String
          }
        """

      override val typeMappings =
        List(
          ObjectMapping(
            schema.ref("Query"),
            List(
              CursorField[String]("foo", _ => ???, Nil)
            )
          ),
          ObjectMapping(
            schema.ref("Foo"),
            List(
              CursorField[String]("bar", _ => ???, Nil),
              CursorField[String]("baz", _ => ???, Nil, hidden = true),
            ),
          )
        )
    }

    val es = M.validate()
    es match {
      case Nil => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("declared fields must not be hidden") {

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo: Foo
          }

          type Foo {
            bar: String
          }
        """

      override val typeMappings =
        List(
          ObjectMapping(
            schema.ref("Query"),
            List(
              CursorField[String]("foo", _ => ???, Nil)
            )
          ),
          ObjectMapping(
            schema.ref("Foo"),
            List(
              CursorField[String]("bar", _ => ???, Nil, hidden = true),
            ),
          )
        )
    }

    val es = M.validate()
    es match {
      case List(M.DeclaredFieldMappingIsHidden(_, _)) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }
  }


  test("unsafeValidate") {
    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo: Foo
          }

          scalar Foo
        """

      override val typeMappings =
        TypeMappings.unchecked(
          List(
            ObjectMapping(
              schema.ref("Query"),
              List(
                CursorField[String]("foo", _ => ???, Nil)
              )
            ),
            ObjectMapping(schema.uncheckedRef("Bar"), Nil)
          )
        )
    }

    intercept[ValidationException] {
      M.unsafeValidate()
    }
  }
}
