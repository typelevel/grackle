// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import cats.Id
import cats.implicits._
import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import Query._, Predicate._, Value._
import QueryCompiler._
import QueryInterpreter.mkOneError
import semiauto._

object MutualRecursionData {
  val schema =
    Schema(
      """
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
    ).right.get

  val QueryType = schema.ref("Query")
  val ProgrammeType = schema.ref("Programme")
  val ProductionType = schema.ref("Production")

  case class Programme(id: String, productions: Option[List[String]])
  object Programme {
    implicit val cursorBuilder: CursorBuilder[Programme] =
      deriveObjectCursorBuilder[Programme].transformField("productions")(resolveProductions)

    def resolveProductions(p: Programme): Result[Option[List[Production]]] =
      p.productions match {
        case None => None.rightIor
        case Some(ids) =>
          ids.traverse(id => productions.find(_.id == id).toRightIor(mkOneError(s"Bad id '$id'"))).map(_.some)
      }
  }

  case class Production(id: String, programme: String)
  object Production {
    implicit val cursorBuilder: CursorBuilder[Production] =
      deriveObjectCursorBuilder[Production].transformField("programme")(resolveProgramme)

    def resolveProgramme(p: Production): Result[Programme] = {
      val id = p.programme
      programmes.find(_.id == id).toRightIor(mkOneError(s"Bad id '$id'"))
    }
  }

  val programmes = List(Programme("prog1", Some(List("prod1"))))
  val productions = List(Production("prod1", "prog1"))
}

import MutualRecursionData._

object MutualRecursionQueryCompiler extends QueryCompiler(schema) {
  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select(f@("programmeById" | "productionById"), List(Binding("id", IDValue(id))), child) =>
        Select(f, Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

object MutualRecursionQueryInterpreter extends GenericQueryInterpreter[Id](
  {
    case "programmeById" =>
      CursorBuilder[List[Programme]].build(programmes, ListType(ProgrammeType))
    case "productionById" =>
      CursorBuilder[List[Production]].build(productions, ListType(ProductionType))
  },
)

final class MutualRecursionSpec extends CatsSuite {
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

    val compiledQuery = MutualRecursionQueryCompiler.compile(query).right.get
    val res = MutualRecursionQueryInterpreter.run(compiledQuery, MutualRecursionData.schema.queryType)
    //println(res)

    assert(res == expected)
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

    val compiledQuery = MutualRecursionQueryCompiler.compile(query).right.get
    val res = MutualRecursionQueryInterpreter.run(compiledQuery, MutualRecursionData.schema.queryType)
    //println(res)

    assert(res == expected)
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

    val compiledQuery = MutualRecursionQueryCompiler.compile(query).right.get
    val res = MutualRecursionQueryInterpreter.run(compiledQuery, MutualRecursionData.schema.queryType)
    //println(res)

    assert(res == expected)
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

    val compiledQuery = MutualRecursionQueryCompiler.compile(query).right.get
    val res = MutualRecursionQueryInterpreter.run(compiledQuery, MutualRecursionData.schema.queryType)
    //println(res)

    assert(res == expected)
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

    val compiledQuery = MutualRecursionQueryCompiler.compile(query).right.get
    val res = MutualRecursionQueryInterpreter.run(compiledQuery, MutualRecursionData.schema.queryType)
    //println(res)

    assert(res == expected)
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

    val compiledQuery = MutualRecursionQueryCompiler.compile(query).right.get
    val res = MutualRecursionQueryInterpreter.run(compiledQuery, MutualRecursionData.schema.queryType)
    //println(res)

    assert(res == expected)
  }
}
