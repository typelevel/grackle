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

package grackle
package generic

import cats.effect.IO
import cats.implicits._
import io.circe.literal._
import munit.CatsEffectSuite

import grackle.syntax._
import Query._, Predicate._, Value._
import QueryCompiler._

object MutualRecursionData {
  import MutualRecursionMapping._
  import semiauto._

  case class Programme(id: String, productions: Option[List[String]])
  object Programme {
    implicit val cursorBuilder: CursorBuilder[Programme] =
      deriveObjectCursorBuilder[Programme](ProgrammeType)
        .transformField("productions")(resolveProductions)

    def resolveProductions(p: Programme): Result[Option[List[Production]]] =
      p.productions match {
        case None => None.success
        case Some(ids) =>
          ids.traverse(id => productions.find(_.id == id).toResultOrError(s"Bad id '$id'")).map(_.some)
      }
  }

  case class Production(id: String, programme: String)
  object Production {
    implicit val cursorBuilder: CursorBuilder[Production] =
      deriveObjectCursorBuilder[Production](ProductionType)
        .transformField("programme")(resolveProgramme)

    def resolveProgramme(p: Production): Result[Programme] = {
      val id = p.programme
      programmes.find(_.id == id).toResultOrError(s"Bad id '$id'")
    }
  }

  val programmes = List(Programme("prog1", Some(List("prod1"))))
  val productions = List(Production("prod1", "prog1"))
}

object MutualRecursionMapping extends GenericMapping[IO] {
  import MutualRecursionData._

  val schema =
    schema"""
      type Query {
        programmeById(id: ID!): Programme
        productionById(id: ID!): Production
      }
      type Programme {
        id: String!
        productions: [Production!]
      }
      type Production {
        id: String!
        programme: Programme!
      }
    """

  val QueryType = schema.ref("Query")
  val ProgrammeType = schema.ref("Programme")
  val ProductionType = schema.ref("Production")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            GenericField("programmeById", programmes),
            GenericField("productionById", productions)
          )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "programmeById" | "productionById", List(Binding("id", IDValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(ProgrammeType / "id", Const(id)), child)))
  }
}

final class RecursionSuite extends CatsEffectSuite {
  test("simple query") {
    val query = """
      query {
        programmeById(id: "prog1") {
          id
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "programmeById" : {
            "id" : "prog1"
          }
        }
      }
    """

    val res = MutualRecursionMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("mutually recursive query (1)") {
    val query = """
      query {
        programmeById(id: "prog1") {
          id
          productions {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "programmeById" : {
            "id" : "prog1",
            "productions" : [
              {
                "id" : "prod1"
              }
            ]
          }
        }
      }
    """

    val res = MutualRecursionMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("mutually recursive query (2)") {
    val query = """
      query {
        programmeById(id: "prog1") {
          id
          productions {
            programme {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "programmeById" : {
            "id" : "prog1",
            "productions" : [
              {
                "programme" : {
                  "id" : "prog1"
                }
              }
            ]
          }
        }
      }
    """

    val res = MutualRecursionMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("mutually recursive query (3)") {
    val query = """
      query {
        programmeById(id: "prog1") {
          id
          productions {
            programme {
              id
              productions {
                id
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "programmeById" : {
            "id" : "prog1",
            "productions" : [
              {
                "programme" : {
                  "id" : "prog1",
                  "productions" : [
                    {
                      "id" : "prod1"
                    }
                  ]
                }
              }
            ]
          }
        }
      }
    """

    val res = MutualRecursionMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("mutually recursive query (4)") {
    val query = """
      query {
        productionById(id: "prod1") {
          id
          programme {
            id
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "productionById" : {
            "id" : "prod1",
            "programme" : {
              "id" : "prog1"
            }
          }
        }
      }
    """

    val res = MutualRecursionMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("mutually recursive query (5)") {
    val query = """
      query {
        productionById(id: "prod1") {
          id
          programme {
            id
            productions {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "productionById" : {
            "id" : "prod1",
            "programme" : {
              "id" : "prog1",
              "productions" : [
                {
                  "id" : "prod1"
                }
              ]
            }
          }
        }
      }
    """

    val res = MutualRecursionMapping.compileAndRun(query)

    assertIO(res, expected)
  }
}
