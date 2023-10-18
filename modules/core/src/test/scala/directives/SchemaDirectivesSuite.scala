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

package directives

import cats.effect.IO
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import Cursor._, Query._, QueryCompiler._, Value._

import SchemaDirectivesMapping.AuthStatus

final class SchemaDirectivesSuite extends CatsEffectSuite {
  test("No auth, success") {
    val query = """
      query {
        products
      }
    """

    val expected = json"""
      {
        "data" : {
          "products" : [
            "Cheese",
            "Wine",
            "Bread"
          ]
        }
      }
    """

    val res = SchemaDirectivesMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("No auth, fail") {
    val query = """
      query {
        products
        user {
          name
          email
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Unauthorized"
          }
        ]
      }
    """

    val res = SchemaDirectivesMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("Authenticated user, success") {
    val query = """
      query {
        products
        user {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "products" : [
            "Cheese",
            "Wine",
            "Bread"
          ],
          "user" : {
            "name" : "Mary"
          }
        }
      }
    """

    val res = SchemaDirectivesMapping.compileAndRun(query, env = Env("authStatus" -> AuthStatus("USER")))

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("Authenticated user, fail") {
    val query = """
      query {
        products
        user {
          name
          email
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Unauthorized"
          }
        ]
      }
    """

    val res = SchemaDirectivesMapping.compileAndRun(query, env = Env("authStatus" -> AuthStatus("USER")))

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("Authenticated admin, success") {
    val query = """
      query {
        products
        user {
          name
          email
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "products" : [
            "Cheese",
            "Wine",
            "Bread"
          ],
          "user" : {
            "name" : "Mary",
            "email" : "mary@example.com"
          }
        }
      }
    """

    val res = SchemaDirectivesMapping.compileAndRun(query, env = Env("authStatus" -> AuthStatus("ADMIN")))

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("Authenticated user, warn with null") {
    val query = """
      query {
        products
        user {
          phone
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Unauthorized access to field 'phone' of type 'User'"
          }
        ],
        "data" : {
          "products" : [
            "Cheese",
            "Wine",
            "Bread"
          ],
          "user" : {
            "phone" : null
          }
        }
      }
    """

    val res = SchemaDirectivesMapping.compileAndRun(query, env = Env("authStatus" -> AuthStatus("USER")))

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("Authenticated admin, success with non-null") {
    val query = """
      query {
        products
        user {
          phone
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "products" : [
            "Cheese",
            "Wine",
            "Bread"
          ],
          "user" : {
            "phone" : "123456789"
          }
        }
      }
    """

    val res = SchemaDirectivesMapping.compileAndRun(query, env = Env("authStatus" -> AuthStatus("ADMIN")))

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("Mutation, authenticated user, success") {
    val query = """
      mutation {
        needsBackup
      }
    """

    val expected = json"""
      {
        "data" : {
          "needsBackup" : true
        }
      }
    """

    val res = SchemaDirectivesMapping.compileAndRun(query, env = Env("authStatus" -> AuthStatus("USER")))

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("Mutation, no auth, fail") {
    val query = """
      mutation {
        needsBackup
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Unauthorized"
          }
        ]
      }
    """

    val res = SchemaDirectivesMapping.compileAndRun(query)

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("Mutation, authenticated admin, success") {
    val query = """
      mutation {
        backup
      }
    """

    val expected = json"""
      {
        "data" : {
          "backup" : true
        }
      }
    """

    val res = SchemaDirectivesMapping.compileAndRun(query, env = Env("authStatus" -> AuthStatus("ADMIN")))

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }

  test("Mutation, authenticated user, fail") {
    val query = """
      mutation {
        backup
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Unauthorized"
          }
        ]
      }
    """

    val res = SchemaDirectivesMapping.compileAndRun(query, env = Env("authStatus" -> AuthStatus("USER")))

    //res.flatMap(IO.println) *>
    assertIO(res, expected)
  }
}

object SchemaDirectivesMapping extends ValueMapping[IO] {
  val schema =
    schema"""
      type Query {
        products: [String!]!
        user: User! @authenticated
      }

      type Mutation @authenticated {
        needsBackup: Boolean!
        backup: Boolean! @hasRole(requires: ADMIN)
      }

      type User {
        name: String!
        email: String! @hasRole(requires: ADMIN)
        phone: String  @hasRole(requires: ADMIN)
      }

      directive @authenticated on FIELD_DEFINITION | OBJECT
      directive @hasRole(requires: Role = ADMIN) on FIELD_DEFINITION | OBJECT

      enum Role {
        ADMIN
        USER
      }
    """

  val QueryType = schema.ref("Query")
  val MutationType = schema.ref("Mutation")
  val UserType = schema.ref("User")

  val typeMappings =
    List(
      ValueObjectMapping[Unit](
        tpe = QueryType,
        fieldMappings =
          List(
            ValueField("products", _ => List("Cheese", "Wine", "Bread")),
            ValueField("user", _ => ())
          )
      ),
      ValueObjectMapping[Unit](
        tpe = MutationType,
        fieldMappings =
          List(
            ValueField("needsBackup", _ => true),
            ValueField("backup", _ => true)
          )
      ),
      ValueObjectMapping[Unit](
        tpe = UserType,
        fieldMappings =
          List(
            ValueField("name", _ => "Mary"),
            ValueField("email", _ => "mary@example.com"),
            ValueField("phone", _ => Some("123456789")),
          )
      )
    )

  case class AuthStatus(role: String)

  object permissionsElaborator extends Phase {
    override def transform(query: Query): Elab[Query] = {
      def checkPermissions(c: Context, name: String, status: Option[AuthStatus], query: Query, nullAndWarn: Boolean): Elab[Query] = {
        val dirs = c.tpe.directives ++ c.tpe.fieldInfo(name).map(_.directives).getOrElse(Nil)
        val requiresAuth = dirs.exists(_.name == "authenticated")
        val roles =
          dirs.filter(_.name == "hasRole").flatMap(_.args.filter(_.name == "requires")).map(_.value).collect {
            case EnumValue(role) => role
          }
        val requiresRole =
          if (roles.contains("ADMIN")) Some(AuthStatus("ADMIN"))
          else if (roles.contains("USER")) Some(AuthStatus("USER"))
          else None

        (status, requiresAuth, requiresRole) match {
          case (None, false, None) => Elab.pure(query)
          case (Some(_), _, None) => Elab.pure(query)
          case (Some(AuthStatus("ADMIN")), _, _) => Elab.pure(query)
          case (Some(AuthStatus("USER")), _, Some(AuthStatus("USER"))) =>
            Elab.pure(query)
          case _ =>
            if (!nullAndWarn) Elab.failure(s"Unauthorized")
            else
              for {
                _ <- Elab.warning(s"Unauthorized access to field '$name' of type '${c.tpe}'")
              } yield TransformCursor(NullFieldCursor(_).success, query)
        }
      }

      query match {
        case s: UntypedSelect =>
          for {
            c      <- Elab.context
            status <- Elab.env[AuthStatus]("authStatus")
            query0 <- super.transform(query)
            res    <- checkPermissions(c, s.name, status, query0, s.name == "phone")
          } yield res

        case _ =>
          super.transform(query)
      }
    }
  }

  override def compilerPhases: List[QueryCompiler.Phase] =
    List(permissionsElaborator, selectElaborator, componentElaborator, effectElaborator)
}
