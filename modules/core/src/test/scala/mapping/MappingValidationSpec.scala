// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package mapping

import cats.Id
import cats.data.Chain
import cats.implicits._
import cats.tests.CatsSuite
import io.circe.Json

import edu.gemini.grackle.{Schema, ValueMapping}

import compiler.MovieMapping
import composed.CountryData

class MappingValidationSpec extends CatsSuite {

  test("validate ValueMapping") {
    val expected = Chain.empty[Json]

    val res = MovieMapping.validate

    assert(res == expected)
  }

  test("validate missing typeMapping") {
    val expected =
      Chain(
        Json.fromString("Found mapping for unknown type Country"),
        Json.fromString("Found mapping for unknown field code of type Country")
      )

    val res = UnvalidatedTypeMapping.validate

    assert(res == expected)
  }

  test("validate missing fieldMapping") {
    val expected =
      Chain(
        Json.fromString("Found mapping for unknown field name of type Country")
      )

    val res = UnvalidatedFieldMapping.validate

    assert(res == expected)
  }

  test("validate missing cursor field/attribute mapping") {
    val expected =
      Chain(
        Json.fromString("Found mapping for unknown field computedField0 of type Country"),
        Json.fromString("Found mapping for unknown field computedField1 of type Country"),
        Json.fromString("No field/attribute mapping for missingField in object mapping for Country"),
        Json.fromString("No field/attribute mapping for missingAttr in object mapping for Country")
      )
    val res = UnvalidatedCursorMapping.validate

    assert(res == expected)
  }
}

object UnvalidatedTypeMapping extends ValueMapping[Id] {
  import CountryData._

  val schema =
    Schema(
      """
        type Query {
          country(code: String): Country
        }
      """
    ).right.get

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            ValueRoot("country", countries)
          )
      ),
      ValueObjectMapping[Country](
        tpe = CountryType,
        fieldMappings =
          List(
            ValueField("code", _.code)
          )
      )
    )
}

object UnvalidatedFieldMapping extends ValueMapping[Id] {
  import CountryData._

  val schema =
    Schema(
      """
        type Query {
          country(code: String): Country
        }
        type Country {
          code: String!
        }
      """
    ).right.get

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            ValueRoot("country", countries)
          )
      ),
      ValueObjectMapping[Country](
        tpe = CountryType,
        fieldMappings =
          List(
            ValueField("code", _.code),
            ValueField("name", _.name),
            ValueAttribute("attr", _ => ()) // Attribute, so don't report as missing from schema
          )
      )
    )
}

object UnvalidatedCursorMapping extends ValueMapping[Id] {
  import CountryData._

  val schema =
    Schema(
      """
        type Query {
          country(code: String): Country
        }
        type Country {
          code: String!
          computed: String!
        }
      """
    ).right.get

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            ValueRoot("country", countries)
          )
      ),
      ValueObjectMapping[Country](
        tpe = CountryType,
        fieldMappings =
          List(
            ValueField("code", _.code),
            ValueAttribute("attr", x => x),
            CursorField("computedField0", _ => ().rightIor, List("code")),
            CursorField("computedField1", _ => ().rightIor, List("missingField")),
            CursorAttribute("computedAttr0", _ => ().rightIor, List("attr")),
            CursorAttribute("computedAttr1", _ => ().rightIor, List("missingAttr"))
          )
      )
    )
}
