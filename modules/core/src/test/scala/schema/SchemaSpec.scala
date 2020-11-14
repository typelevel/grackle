// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package schema

import cats.data.Ior
import cats.data.Ior.Both
import cats.tests.CatsSuite
import edu.gemini.grackle.Schema

final class SchemaSpec extends CatsSuite {
  test("schema validation: undefined types: typo in the use of a Query result type") {
    val schema =
      Schema(
      """
         type Query {
          episodeById(id: String!): Episod
         }

         type Episode {
          id: String!
        }
    """
    )

    schema match {
      case Ior.Left(e) => assert(e.head.\\("message").head.asString.get == "Reference to undefined type: Episod")
      case Both(a, b)  =>
        assert(a.head.\\("message").head.asString.get == "Reference to undefined type: Episod")
        assert(b.types.map(_.name) == List("Query", "Episode"))
      case Ior.Right(b) => fail(s"Shouldn't compile: $b")
    }
  }

  test("schema validation: undefined types: typo in the use of an InputValueDefinition") {
    val schema = Schema(
      """
         type Query {
          episodeById(id: CCid!): Episode
         }

         scalar CCId

         type Episode {
          id: CCId!
        }
    """
    )

    schema match {
      case Both(a, b)  =>
        assert(a.head.\\("message").head.asString.get == "Reference to undefined type: CCid")
        assert(b.types.map(_.name) == List("Query", "CCId", "Episode"))
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: multiply-defined types") {
    val schema = Schema(
      """
         type Query {
          episodeById(id: String!): Episode
         }

         type Episode {
           id: String!
         }

         type Episode {
          episodeId: String!
        }
    """
    )

    schema match {
      case Ior.Left(e) => assert(e.head.\\("message").head.asString.get == "Duplicate NamedType found: Episode")
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }

  test("schema validation: multiple deprecated annotations") {
    val schema = Schema(
      """
         type ExampleType {
          oldField: String @deprecated @deprecated
        }
    """
    )

    schema match {
      case Ior.Left(e) => assert(e.head.\\("message").head.asString.get == "Only a single deprecated allowed at a given location")
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }


  test("schema validation: deprecated annotation with unsupported argument") {
    val schema = Schema(
      """
         type ExampleType {
          oldField: String @deprecated(notareadon: "foo bar baz")
        }
    """
    )

    schema match {
      case Ior.Left(e) => assert(e.head.\\("message").head.asString.get == "deprecated must have a single String 'reason' argument, or no arguments")
      case unexpected => fail(s"This was unexpected: $unexpected")
    }
  }
}
