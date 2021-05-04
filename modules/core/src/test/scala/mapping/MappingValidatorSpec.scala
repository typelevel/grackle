// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package validator

import org.scalatest.funsuite.AnyFunSuite
import edu.gemini.grackle.Mapping
import cats.Id
import cats.syntax.all._
import cats.catsInstancesForId
import edu.gemini.grackle.{ ListType, MappingValidator }
import edu.gemini.grackle.MappingValidator.ValidationException
import edu.gemini.grackle.syntax._

final class ValidatorSpec extends AnyFunSuite {

  test("missing type mapping") {

    object M extends Mapping[Id] {
      val schema = schema"type Foo { bar: String }"
      val typeMappings = Nil
    }

    val es = M.validator.validateMapping()
    es match {
      case List(M.validator.MissingTypeMapping(_)) => succeed
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("missing field mapping") {

    object M extends Mapping[Id] {
      val schema = schema"type Foo { bar: String }"
      val typeMappings  = List(ObjectMapping(schema.ref("Foo"), Nil))
    }

    val es = M.validator.validateMapping()
    es match {
      case List(M.validator.MissingFieldMapping(_, f)) if f.name == "bar" => succeed
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("inapplicable type (object mapping for scalar)") {

    object M extends Mapping[Id] {
      val schema = schema"scalar Foo"
      val typeMappings = List(ObjectMapping(schema.ref("Foo"), Nil))
    }

    val es = M.validator.validateMapping()
    es match {
      case List(M.validator.InapplicableGraphQLType(_, _)) => succeed
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("inapplicable type (leaf mapping for object)") {

    object M extends Mapping[Id] {
      val schema =  schema"type Foo { bar: String }"
      val typeMappings = List(LeafMapping[String](schema.ref("Foo")))
    }

    val es = M.validator.validateMapping()
    es match {
      case List(M.validator.InapplicableGraphQLType(_, _)) => succeed
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("enums are valid leaf mappings") {

    object M extends Mapping[Id] {
      val schema = schema"enum Foo { BAR }"
      val typeMappings = List(LeafMapping[String](schema.ref("Foo")))
    }

    val es = M.validator.validateMapping()
    es match {
      case Nil => succeed
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("lists are valid leaf mappings") {

    object M extends Mapping[Id] {
      val schema = schema"enum Foo { BAR } type Baz { quux: [Foo] }"
      val typeMappings = List(
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
      case Nil => succeed
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("input object types don't require mappings") {

    object M extends Mapping[Id] {
      val schema = schema"input Foo { bar: String }"
      val typeMappings = Nil
    }

    val es = M.validator.validateMapping()
    es match {
      case Nil => succeed
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("input only enums are valid primitive mappings") {

    object M extends Mapping[Id] {
      val schema = schema"input Foo { bar: Bar } enum Bar { BAZ }"
      val typeMappings = List(
        PrimitiveMapping(schema.ref("Bar"))
      )
    }

    val es = M.validator.validateMapping()
    es match {
      case Nil => succeed
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }


  test("nonexistent type (type mapping)") {

    object M extends Mapping[Id] {
      val schema = schema""
      val typeMappings  = List(ObjectMapping(schema.ref("Foo"), Nil))
    }

    val es = M.validator.validateMapping()
    es match {
      case List(M.validator.ReferencedTypeDoesNotExist(_)) => succeed
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("unknown field") {

    object M extends Mapping[Id] {
      val schema = schema"type Foo { bar: String }"
      val typeMappings = List(
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
      case List(M.validator.ReferencedFieldDoesNotExist(_, _)) => succeed
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("non-field attributes are valid") {

    object M extends Mapping[Id] {
      val schema = schema"type Foo { bar: String }"
      val typeMappings = List(
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
      case Nil => succeed
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("unsafeValidate") {
    object M extends Mapping[Id] {
      val schema = schema"scalar Bar"
      val typeMappings = List(ObjectMapping(schema.ref("Foo"), Nil))
    }
    intercept[ValidationException] {
      MappingValidator(M).unsafeValidate()
    }
  }

}
