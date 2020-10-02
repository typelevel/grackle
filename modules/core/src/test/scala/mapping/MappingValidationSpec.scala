// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package mapping

import cats.Id
import cats.tests.CatsSuite
import compiler.MovieMapping
import composed.CountryData
import edu.gemini.grackle.{Schema, ValueMapping}

class MappingValidationSpec extends CatsSuite {

  test("validate ValueMapping") {
    assert(MovieMapping.validate.isRight)
  }

  test("validate missing typeMapping") {
    val result = UnvalidatedTypeMapping.validate
    assert(result.isLeft && result.left.toOption.get.head.noSpaces.contains("Country"))
  }

  test("validate missing fieldMapping") {
    val result = UnvalidatedFieldMapping.validate
    assert(result.isLeft && result.left.toOption.get.head.noSpaces.contains("name"))
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
            ValueField("name", _.name)
          )
      )
    )
}