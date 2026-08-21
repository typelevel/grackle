// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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

package conformance

import cats.effect.IO

import grackle._
import grackle.Predicate.{Const, Eql, In}
import grackle.Query.{Binding, Filter, Unique}
import grackle.QueryCompiler._
import grackle.Value.{IntValue, ListValue, StringValue}
import grackle.syntax._

/**
 * The mapping for the examples of section 1, Overview, and section 2, Language.
 *
 * The specification defines no schema for these examples. The schema here holds the types and
 * the fields which the examples select, and the data holds the values which the specification
 * states in its response examples.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Language
 */
object LanguageMappings {

  sealed trait Profile {
    def handle: String
  }

  case class Connection(count: Int)

  case class User(id: Int, name: String, handle: String, friends: Connection) extends Profile

  case class Page(handle: String, likers: Connection) extends Profile

  /**
   * The two profiles which the examples name, in the order of the example of section 2.9.1.
   */
  val profiles: List[Profile] =
    List(
      User(4, "Mark Zuckerberg", "zuck", Connection(1234)),
      Page("coca-cola", Connection(90234512))
    )

  val users: List[User] = profiles.collect { case u: User => u }

  /**
   * The picture of the user `id` at `size`, in the form which section 2.8 states.
   */
  def profilePic(id: Int, size: Int): String =
    s"https://cdn.site.io/pic-$id-$size.jpg"

  object Site extends ValueMapping[IO] {
    val schema =
      schema"""
        type Query {
          user(id: Int!): User
          profiles(handles: [String!]!): [Profile!]!
        }
        interface Profile {
          handle: String!
        }
        type User implements Profile {
          id: Int!
          name: String!
          handle: String!
          profilePic(size: Int): String!
          friends: Connection!
        }
        type Page implements Profile {
          handle: String!
          likers: Connection!
        }
        type Connection {
          count: Int!
        }
      """

    val QueryType = schema.ref("Query")
    val ProfileType = schema.ref("Profile")
    val UserType = schema.ref("User")
    val PageType = schema.ref("Page")
    val ConnectionType = schema.ref("Connection")

    val typeMappings =
      List(
        ValueObjectMapping[Unit](
          tpe = QueryType,
          fieldMappings = List(
            ValueField("user", _ => users),
            ValueField("profiles", _ => profiles)
          )),
        ValueObjectMapping[Profile](
          tpe = ProfileType,
          fieldMappings = List(ValueField("handle", _.handle))),
        ValueObjectMapping[User](
          tpe = UserType,
          fieldMappings = List(
            ValueField("id", _.id),
            ValueField("name", _.name),
            ValueField("friends", _.friends),
            CursorField("profilePic", picture)
          )),
        ValueObjectMapping[Page](
          tpe = PageType,
          fieldMappings = List(ValueField("likers", _.likers))),
        ValueObjectMapping[Connection](
          tpe = ConnectionType,
          fieldMappings = List(ValueField("count", _.count)))
      )

    override val selectElaborator: SelectElaborator =
      SelectElaborator {
        case (QueryType, "user", List(Binding("id", IntValue(id)))) =>
          Elab.transformChild(child => Unique(Filter(Eql(UserType / "id", Const(id)), child)))
        case (QueryType, "profiles", List(Binding("handles", ListValue(handles)))) =>
          val hs = handles.collect { case StringValue(h) => h }
          Elab.transformChild(child => Filter(In(ProfileType / "handle", hs), child))
        case (UserType, "profilePic", List(Binding("size", IntValue(size)))) =>
          Elab.env("size" -> size)
      }

    private def picture(c: Cursor): Result[String] =
      for {
        user <- c.as[User]
        size <- c.envR[Int]("size")
      } yield profilePic(user.id, size)
  }
}
