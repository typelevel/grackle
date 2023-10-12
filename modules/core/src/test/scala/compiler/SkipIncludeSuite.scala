// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import Query._

final class SkipIncludeSuite extends CatsEffectSuite {
  test("skip/include field") {
    val query = """
      query ($yup: Boolean, $nope: Boolean) {
        a: field @skip(if: $yup) {
          subfieldA
        }
        b: field @skip(if: $nope) {
          subfieldB
        }
        c: field @include(if: $yup) {
          subfieldA
        }
        d: field @include(if: $nope) {
          subfieldB
        }
      }
    """

    val variables = json"""
      {
        "yup": true,
        "nope": false
      }
    """

    val expected =
      Group(List(
        Select("field", Some("b"), Select("subfieldB")),
        Select("field", Some("c"), Select("subfieldA"))
      ))

    val compiled = SkipIncludeMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("skip/include fragment spread") {
    val query = """
      query ($yup: Boolean, $nope: Boolean) {
        a: field {
          ...frag @skip(if: $yup)
        }
        b: field {
          ...frag @skip(if: $nope)
        }
        c: field {
          ...frag @include(if: $yup)
        }
        d: field {
          ...frag @include(if: $nope)
        }
      }

      fragment frag on Value {
        subfieldA
        subfieldB
      }
    """

    val variables = json"""
      {
        "yup": true,
        "nope": false
      }
    """

    val expected =
      Group(List(
        Select("field", Some("a")),
        Select("field", Some("b"),
          Group(List(
            Select("subfieldA"),
            Select("subfieldB")
          ))
        ),
        Select("field", Some("c"),
          Group(List(
            Select("subfieldA"),
            Select("subfieldB")
          ))
        ),
        Select("field", Some("d"))
      ))

    val compiled = SkipIncludeMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("fragment spread with nested skip/include") {
    val query = """
      query ($yup: Boolean, $nope: Boolean) {
        field {
          ...frag
        }
      }

      fragment frag on Value {
        a: subfieldA @skip(if: $yup)
        b: subfieldB @skip(if: $nope)
        c: subfieldA @include(if: $yup)
        d: subfieldB @include(if: $nope)
      }
    """

    val variables = json"""
      {
        "yup": true,
        "nope": false
      }
    """

    val expected =
      Select("field",
        Group(List(
          Select("subfieldB", Some("b")),
          Select("subfieldA", Some("c"))
        ))
      )

    val compiled = SkipIncludeMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("skip/include inline fragment") {
    val query = """
      query ($yup: Boolean, $nope: Boolean) {
        a: field {
          ... on Value @skip(if: $yup) {
            subfieldA
            subfieldB
          }
        }
        b: field {
          ... on Value @skip(if: $nope) {
            subfieldA
            subfieldB
          }
        }
        c: field {
          ... on Value @include(if: $yup) {
            subfieldA
            subfieldB
          }
        }
        d: field {
          ... on Value @include(if: $nope) {
            subfieldA
            subfieldB
          }
        }
      }
    """

    val variables = json"""
      {
        "yup": true,
        "nope": false
      }
    """

    val expected =
      Group(List(
        Select("field", Some("a")),
        Select("field", Some("b"),
          Group(List(
            Select("subfieldA"),
            Select("subfieldB")
          ))
        ),
        Select("field", Some("c"),
          Group(List(
            Select("subfieldA"),
            Select("subfieldB")
          ))
        ),
        Select("field", Some("d"))
      ))

    val compiled = SkipIncludeMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
  }

  test("inline fragment with nested skip/include") {
    val query = """
      query ($yup: Boolean, $nope: Boolean) {
        field {
          ... on Value {
            a: subfieldA @skip(if: $yup)
            b: subfieldB @skip(if: $nope)
            c: subfieldA @include(if: $yup)
            d: subfieldB @include(if: $nope)
          }
        }
      }
    """

    val variables = json"""
      {
        "yup": true,
        "nope": false
      }
    """

    val expected =
      Select("field",
        Group(List(
          Select("subfieldB", Some("b")),
          Select("subfieldA", Some("c"))
        ))
      )

    val compiled = SkipIncludeMapping.compiler.compile(query, untypedVars = Some(variables))

    assertEquals(compiled.map(_.query), Result.Success(expected))
  }
}

object SkipIncludeMapping extends TestMapping {
  val schema =
    schema"""
      type Query {
        field: Value!
      }
      type Value {
        subfieldA: String
        subfieldB: String
      }
    """
}
