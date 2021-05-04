// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.Id
import cats.data.Ior
import cats.tests.CatsSuite
import cats.catsInstancesForId

import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import Query._

final class SkipIncludeSuite extends CatsSuite {
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
        Rename("b", Select("field", Nil, Select("subfieldB", Nil, Empty))),
        Rename("c", Select("field", Nil, Select("subfieldA", Nil, Empty)))
      ))

    val compiled = SkipIncludeMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)

    assert(compiled.map(_.query) == Ior.Right(expected))
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

    val compiled = SkipIncludeMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)

    assert(compiled.map(_.query) == Ior.Right(expected))
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
      Select("field", Nil,
        Group(List(
          Rename("b", Select("subfieldB", Nil, Empty)),
          Rename("c", Select("subfieldA", Nil, Empty))
        ))
      )

    val compiled = SkipIncludeMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)

    assert(compiled.map(_.query) == Ior.Right(expected))
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

    val compiled = SkipIncludeMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)

    assert(compiled.map(_.query) == Ior.Right(expected))
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
      Select("field", Nil,
        Group(List(
          Rename("b", Select("subfieldB", Nil, Empty)),
          Rename("c", Select("subfieldA", Nil, Empty))
        ))
      )

    val compiled = SkipIncludeMapping.compiler.compile(query, untypedVars = Some(variables))
    //println(compiled)

    assert(compiled.map(_.query) == Ior.Right(expected))
  }
}

object SkipIncludeMapping extends Mapping[Id] {
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

  val typeMappings = Nil
}
