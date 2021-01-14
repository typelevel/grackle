// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package validator

import org.scalatest.funsuite.AnyFunSuite
import edu.gemini.grackle.Mapping
import cats.Id
import edu.gemini.grackle.Schema
import edu.gemini.grackle.MappingValidator
import edu.gemini.grackle.MappingValidator.{ Severity, ValidationException }

final class ValidatorSpec extends AnyFunSuite {

  test("missing field mapping") {

    object M extends Mapping[Id] {
      val schema = Schema("type Foo { bar: String }").right.get
      val typeMappings  = List(ObjectMapping(schema.ref("Foo"), Nil))
    }

    val v =  MappingValidator(M)
    val es = v.validateMapping()
    es match {
      case List(v.MissingFieldMapping(_, f)) if f.name == "bar" => succeed
      case _ => fail(es.toString())
    }

  }

  test("inapplicable type (object mapping for scalar)") {

    object M extends Mapping[Id] {
      val schema = Schema("scalar Foo").right.get
      val typeMappings = List(ObjectMapping(schema.ref("Foo"), Nil))
    }

    val v =  MappingValidator(M)
    val es = v.validateMapping()
    es match {
      case List(v.InapplicableGraphQLType(_, _)) => succeed
      case _ => fail(es.toString())
    }

  }

  test("inapplicable type (leaf mapping for object)") {

    object M extends Mapping[Id] {
      val schema =  Schema("type Foo { bar: String }").right.get
      val typeMappings = List(LeafMapping[String](schema.ref("Foo")))
    }

    val v =  MappingValidator(M)
    val es = v.validateMapping()
    es match {
      case List(v.InapplicableGraphQLType(_, _)) => succeed
      case _ => fail(es.toString())
    }

  }

  test("nonexistent type (type mapping)") {

    object M extends Mapping[Id] {
      val schema = Schema("scalar Bar").right.get
      val typeMappings  = List(ObjectMapping(schema.ref("Foo"), Nil))
    }

    val v =  MappingValidator(M)
    val es = v.validateMapping()
    es match {
      case List(v.ReferencedTypeDoesNotExist(_)) => succeed
      case _ => fail(es.toString())
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

    val v =  MappingValidator(M)
    val es = v.validateMapping().filter(_.severity == Severity.Error)

    es match {
      case List(v.ReferencedFieldDoesNotExist(_, _)) => succeed
      case _ => fail(es.toString())
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
