// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package directives

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import Query._, QueryCompiler._

final class QueryDirectivesSuite extends CatsEffectSuite {
  test("simple query") {
    val query = """
      query {
        user {
          name
          handle
          age
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "user" : {
            "name" : "Mary",
            "handle" : "mary",
            "age" : 42
          }
        }
      }
    """

    val res = QueryDirectivesMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("query with directive (1)") {
    val query = """
      query {
        user {
          name @upperCase
          handle
          age
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "user" : {
            "name" : "MARY",
            "handle" : "mary",
            "age" : 42
          }
        }
      }
    """

    val res = QueryDirectivesMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("query with directive (2)") {
    val query = """
      query {
        user {
          name @upperCase
          handle @upperCase
          age
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "user" : {
            "name" : "MARY",
            "handle" : "MARY",
            "age" : 42
          }
        }
      }
    """

    val res = QueryDirectivesMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("query with directive (3)") {
    val query = """
      query {
        user {
          name @upperCase
          handle
          age @upperCase
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "'upperCase' directive may only be applied to fields of type String"
          }
        ],
        "data" : {
          "user" : {
            "name" : "MARY",
            "handle" : "mary",
            "age" : 42
          }
        }
      }
    """

    val res = QueryDirectivesMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }
}

object QueryDirectivesMapping extends ValueMapping[IO] {
  val schema =
    schema"""
      type Query {
        user: User!
      }
      type User {
        name: String!
        handle: String!
        age: Int!
      }
      directive @upperCase on FIELD
    """

  val QueryType = schema.ref("Query")
  val UserType = schema.ref("User")

  val typeMappings =
    List(
      ValueObjectMapping[Unit](
        tpe = QueryType,
        fieldMappings =
          List(
            ValueField("user", _ => ())
          )
      ),
      ValueObjectMapping[Unit](
        tpe = UserType,
        fieldMappings =
          List(
            ValueField("name", _ => "Mary"),
            ValueField("handle", _ => "mary"),
            ValueField("age", _ => 42)
          )
      )
    )

  object upperCaseElaborator extends Phase {
    override def transform(query: Query): Elab[Query] =
      query match {
        case UntypedSelect(nme, alias, _, directives, _) if directives.exists(_.name == "upperCase") =>
          for {
            c    <- Elab.context
            fc   <- Elab.liftR(c.forField(nme, alias))
            res  <- if (fc.tpe =:= ScalarType.StringType)
                      super.transform(query).map(TransformCursor(toUpperCase, _))
                    else
                      // We could make this fail the whole query by yielding Elab.failure here
                      Elab.warning(s"'upperCase' directive may only be applied to fields of type String") *> super.transform(query)
          } yield res
        case _ =>
          super.transform(query)
      }

    def toUpperCase(c: Cursor): Result[Cursor] =
      FieldTransformCursor[String](c, _.toUpperCase.success).success
  }

  override def compilerPhases: List[QueryCompiler.Phase] =
    List(upperCaseElaborator, selectElaborator, componentElaborator, effectElaborator)
}
