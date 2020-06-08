// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.data.Ior
import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import edu.gemini.grackle._
import Query._

final class SkipIncludeSuite extends CatsSuite {
  test("skip/include field") {
    val text = """
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
        Rename("b", Select("field", Nil, Select("subfieldB", Nil, Empty))),
        Rename("c", Select("field", Nil, Select("subfieldA", Nil, Empty)))
      ))

    val compiled = SkipIncludeCompiler.compile(text, Some(variables))
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }

  test("skip/include fragment spread") {
    val text = """
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
        Rename("a", Select("field", Nil, Empty)),
        Rename("b",
          Select("field", Nil,
            Group(List(
              Select("subfieldA", Nil, Empty),
              Select("subfieldB", Nil, Empty)
            ))
          )
        ),
        Rename("c",
          Select("field", Nil,
            Group(List(
              Select("subfieldA", Nil, Empty),
              Select("subfieldB", Nil, Empty)
            ))
          )
        ),
        Rename("d", Select("field", Nil, Empty))
      ))

    val compiled = SkipIncludeCompiler.compile(text, Some(variables))
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }

  test("fragment spread with nested skip/include") {
    val text = """
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
      Select("field", Nil,
        Group(List(
          Rename("b", Select("subfieldB", Nil, Empty)),
          Rename("c", Select("subfieldA", Nil, Empty))
        ))
      )

    val compiled = SkipIncludeCompiler.compile(text, Some(variables))
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }

  test("skip/include inline fragment") {
    val text = """
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
        Rename("a", Select("field", Nil, Empty)),
        Rename("b",
          Select("field", Nil,
            Group(List(
              Select("subfieldA", Nil, Empty),
              Select("subfieldB", Nil, Empty)
            ))
          )
        ),
        Rename("c",
          Select("field", Nil,
            Group(List(
              Select("subfieldA", Nil, Empty),
              Select("subfieldB", Nil, Empty)
            ))
          )
        ),
        Rename("d", Select("field", Nil, Empty))
      ))

    val compiled = SkipIncludeCompiler.compile(text, Some(variables))
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }

  test("inline fragment with nested skip/include") {
    val text = """
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
      Select("field", Nil,
        Group(List(
          Rename("b", Select("subfieldB", Nil, Empty)),
          Rename("c", Select("subfieldA", Nil, Empty))
        ))
      )

    val compiled = SkipIncludeCompiler.compile(text, Some(variables))
    //println(compiled)
    assert(compiled == Ior.Right(expected))
  }
}

object SkipIncludeSchema extends Schema {
  import ScalarType._

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("field", None, Nil, TypeRef("Value"), false, None),
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "Value",
      description = None,
      fields = List(
        Field("subfieldA", None, Nil, StringType, false, None),
        Field("subfieldB", None, Nil, StringType, false, None),
      ),
      interfaces = Nil
    )
  )

  val directives = Nil
}

object SkipIncludeCompiler extends QueryCompiler(SkipIncludeSchema) {
  val phases = Nil
}
