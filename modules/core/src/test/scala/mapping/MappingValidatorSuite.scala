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

import grackle.{ ListType, MappingValidator }
import grackle.MappingValidator.ValidationException
import grackle.syntax._

import compiler.TestMapping

final class ValidatorSuite extends CatsEffectSuite {

  test("missing type mapping") {

    object M extends TestMapping {
      val schema = schema"type Foo { bar: String }"
    }

    val es = M.validator.validateMapping()
    es match {
      case List(M.validator.MissingTypeMapping(_)) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("missing field mapping") {

    object M extends TestMapping {
      val schema = schema"type Foo { bar: String }"
      override val typeMappings  = List(ObjectMapping(schema.ref("Foo"), Nil))
    }

    val es = M.validator.validateMapping()
    es match {
      case List(M.validator.MissingFieldMapping(_, f)) if f.name == "bar" => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("inapplicable type (object mapping for scalar)") {

    object M extends TestMapping {
      val schema = schema"scalar Foo"
      override val typeMappings = List(ObjectMapping(schema.ref("Foo"), Nil))
    }

    val es = M.validator.validateMapping()
    es match {
      case List(M.validator.InapplicableGraphQLType(_, _)) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("inapplicable type (leaf mapping for object)") {

    object M extends TestMapping {
      val schema =  schema"type Foo { bar: String }"
      override val typeMappings = List(LeafMapping[String](schema.ref("Foo")))
    }

    val es = M.validator.validateMapping()
    es match {
      case List(M.validator.InapplicableGraphQLType(_, _)) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("enums are valid leaf mappings") {

    object M extends TestMapping {
      val schema = schema"enum Foo { BAR }"
      override val typeMappings = List(LeafMapping[String](schema.ref("Foo")))
    }

    val es = M.validator.validateMapping()
    es match {
      case Nil => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("lists are valid leaf mappings") {

    object M extends TestMapping {
      val schema = schema"enum Foo { BAR } type Baz { quux: [Foo] }"
      override val typeMappings = List(
        ObjectMapping(
          schema.ref("Baz"),
          List(
            CursorField[String]("quux", _ => ???, Nil)
          )
        ),
        LeafMapping[String](schema.ref("Foo")),
        LeafMapping[String](ListType(schema.ref("Foo")))
      )
    }

    val es = M.validator.validateMapping()
    es match {
      case Nil => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("input object types don't require mappings") {

    object M extends TestMapping {
      val schema = schema"input Foo { bar: String }"
    }

    val es = M.validator.validateMapping()
    es match {
      case Nil => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("input only enums are valid primitive mappings") {

    object M extends TestMapping {
      val schema = schema"input Foo { bar: Bar } enum Bar { BAZ }"
      override val typeMappings = List(
        PrimitiveMapping(schema.ref("Bar"))
      )
    }

    val es = M.validator.validateMapping()
    es match {
      case Nil => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }


  test("nonexistent type (type mapping)") {

    object M extends TestMapping {
      val schema = schema""
      override val typeMappings  = List(ObjectMapping(schema.ref("Foo"), Nil))
    }

    val es = M.validator.validateMapping()
    es match {
      case List(M.validator.ReferencedTypeDoesNotExist(_)) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("unknown field") {

    object M extends TestMapping {
      val schema = schema"type Foo { bar: String }"
      override val typeMappings = List(
        ObjectMapping(
          schema.ref("Foo"),
          List(
            CursorField[String]("bar", _ => ???, Nil),
            CursorField[String]("quz", _ => ???, Nil),
          ),
        )
      )
    }

    val es = M.validator.validateMapping()
    es match {
      case List(M.validator.ReferencedFieldDoesNotExist(_, _)) => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("non-field attributes are valid") {

    object M extends TestMapping {
      val schema = schema"type Foo { bar: String }"
      override val typeMappings = List(
        ObjectMapping(
          schema.ref("Foo"),
          List(
            CursorField[String]("bar", _ => ???, Nil),
            CursorField[String]("baz", _ => ???, Nil, hidden = true),
          ),
        )
      )
    }

    val es = M.validator.validateMapping()
    es match {
      case Nil => ()
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("unsafeValidate") {
    object M extends TestMapping {
      val schema = schema"scalar Bar"
      override val typeMappings = List(ObjectMapping(schema.ref("Foo"), Nil))
    }
    intercept[ValidationException] {
      MappingValidator(M).unsafeValidate()
    }
  }

}
