// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package validator

import org.scalatest.funsuite.AnyFunSuite
import edu.gemini.grackle.Mapping
import cats.Id
import cats.syntax.all._
import edu.gemini.grackle.Schema
import edu.gemini.grackle.MappingValidator
import edu.gemini.grackle.MappingValidator.ValidationException

final class ValidatorSpec extends AnyFunSuite {

  test("missing type mapping") {

    object M extends Mapping[Id] {
      val schema = Schema("type Foo { bar: String }").right.get
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
      val schema = Schema("type Foo { bar: String }").right.get
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
      val schema = Schema("scalar Foo").right.get
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
      val schema =  Schema("type Foo { bar: String }").right.get
      val typeMappings = List(LeafMapping[String](schema.ref("Foo")))
    }

    val es = M.validator.validateMapping()
    es match {
      case List(M.validator.InapplicableGraphQLType(_, _)) => succeed
      case _ => fail(es.foldMap(_.toErrorMessage))
    }

  }

  test("nonexistent type (type mapping)") {

    object M extends Mapping[Id] {
      val schema = Schema("").right.get
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
      val schema = Schema("type Foo { bar: String }").right.get
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

  test("unsafeValidate") {
    object M extends Mapping[Id] {
      val schema = Schema("scalar Bar").right.get
      val typeMappings = List(ObjectMapping(schema.ref("Foo"), Nil))
    }
    intercept[ValidationException] {
      MappingValidator(M).unsafeValidate()
    }
  }

}
