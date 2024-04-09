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

package compiler

import cats.effect.IO
import cats.implicits._
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import Query._
import Predicate._, Value._
import QueryCompiler._

final class FragmentSuite extends CatsEffectSuite {
  def runOperation(op: Result[Operation]): IO[List[Json]] = {
    val op0 = op.toOption.get
    FragmentMapping.interpreter.run(op0.query, op0.rootTpe, Env.empty).evalMap(FragmentMapping.mkResponse).compile.toList
  }

  test("simple fragment query") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            ...friendFields
          }
          mutualFriends {
            ...friendFields
          }
        }
      }

      fragment friendFields on User {
        id
        name
        profilePic
      }
    """

    val expected =
      Select("user",
        Unique(
          Filter(Eql(FragmentMapping.UserType / "id", Const("1")),
            Group(List(
              Select("friends",
                Group(List(
                  Select("id"),
                  Select("name"),
                  Select("profilePic")
                ))
              ),
              Select("mutualFriends",
                Group(List(
                  Select("id"),
                  Select("name"),
                  Select("profilePic")
                ))
              )
            ))
          )
        )
    )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "friends" : [
              {
                "id" : "2",
                "name" : "Bob",
                "profilePic" : "B"
              },
              {
                "id" : "3",
                "name" : "Carol",
                "profilePic" : "C"
              }
            ],
            "mutualFriends" : [
              {
                "id" : "2",
                "name" : "Bob",
                "profilePic" : "B"
              },
              {
                "id" : "3",
                "name" : "Carol",
                "profilePic" : "C"
              }
            ]
          }
        }
      }
    """

    val compiled = FragmentMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("nested fragment query (1)") {
    val query = """
      query withNestedFragments {
        user(id: 1) {
          friends {
            ...friendFields
          }
          mutualFriends {
            ...friendFields
          }
        }
      }

      fragment friendFields on User {
        id
        name
        ...standardProfilePic
      }

      fragment standardProfilePic on User {
        profilePic
      }
    """

    val expected =
      Select("user",
        Unique(
          Filter(Eql(FragmentMapping.UserType / "id", Const("1")),
            Group(List(
              Select("friends",
                Group(List(
                  Select("id"),
                  Select("name"),
                  Select("profilePic")
                ))
              ),
              Select("mutualFriends",
                Group(List(
                  Select("id"),
                  Select("name"),
                  Select("profilePic")
                ))
              )
            ))
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "friends" : [
              {
                "id" : "2",
                "name" : "Bob",
                "profilePic" : "B"
              },
              {
                "id" : "3",
                "name" : "Carol",
                "profilePic" : "C"
              }
            ],
            "mutualFriends" : [
              {
                "id" : "2",
                "name" : "Bob",
                "profilePic" : "B"
              },
              {
                "id" : "3",
                "name" : "Carol",
                "profilePic" : "C"
              }
            ]
          }
        }
      }
    """

    val compiled = FragmentMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("nested fragment query (2)") {
    val query = """
      query withNestedFragments {
        user(id: 1) {
          friends {
            ...friendFields
          }
          mutualFriends {
            ...friendFields
          }
        }
      }

      fragment friendFields on User {
        id
        ...nameAndStandardProfilePic
      }

      fragment nameAndStandardProfilePic on User {
        name
        ...standardProfilePic
      }

      fragment standardProfilePic on User {
        profilePic
      }
    """

    val expected =
      Select("user",
        Unique(
          Filter(Eql(FragmentMapping.UserType / "id", Const("1")),
            Group(List(
              Select("friends",
                Group(List(
                  Select("id"),
                  Select("name"),
                  Select("profilePic")
                ))
              ),
              Select("mutualFriends",
                Group(List(
                  Select("id"),
                  Select("name"),
                  Select("profilePic")
                ))
              )
            ))
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "friends" : [
              {
                "id" : "2",
                "name" : "Bob",
                "profilePic" : "B"
              },
              {
                "id" : "3",
                "name" : "Carol",
                "profilePic" : "C"
              }
            ],
            "mutualFriends" : [
              {
                "id" : "2",
                "name" : "Bob",
                "profilePic" : "B"
              },
              {
                "id" : "3",
                "name" : "Carol",
                "profilePic" : "C"
              }
            ]
          }
        }
      }
    """

    val compiled = FragmentMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("typed fragment query") {
    val query = """
      query FragmentTyping {
        profiles {
          id
          __typename
          ...userFragment
          ...pageFragment
        }
      }

      fragment userFragment on User {
        name
      }

      fragment pageFragment on Page {
        title
      }
    """

    val User = FragmentMapping.schema.ref("User")
    val Page = FragmentMapping.schema.ref("Page")

    val expected =
      Select("profiles",
        Group(List(
          Select("id"),
          Introspect(FragmentMapping.schema, Select("__typename")),
          Narrow(User, Select("name")),
          Narrow(Page, Select("title"))
        ))
      )

    val expectedResult = json"""
      {
        "data" : {
          "profiles" : [
            {
              "id" : "1",
              "__typename" : "User",
              "name" : "Alice"
            },
            {
              "id" : "2",
              "__typename" : "User",
              "name" : "Bob"
            },
            {
              "id" : "3",
              "__typename" : "User",
              "name" : "Carol"
            },
            {
              "id" : "4",
              "__typename" : "Page",
              "title" : "GraphQL"
            },
            {
              "id" : "5",
              "__typename" : "Page",
              "title" : "Scala"
            }
          ]
        }
      }
    """

    val compiled = FragmentMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("inline fragment query") {
    val query = """
      query inlineFragmentTyping {
        profiles {
          id
          ... on User {
            name
          }
          ... on Page {
            title
          }
        }
      }
    """

    val User = FragmentMapping.schema.ref("User")
    val Page = FragmentMapping.schema.ref("Page")

    val expected =
      Select("profiles",
        Group(List(
          Select("id"),
          Narrow(User, Select("name")),
          Narrow(Page, Select("title"))
        ))
      )

    val expectedResult = json"""
      {
        "data" : {
          "profiles" : [
            {
              "id" : "1",
              "name" : "Alice"
            },
            {
              "id" : "2",
              "name" : "Bob"
            },
            {
              "id" : "3",
              "name" : "Carol"
            },
            {
              "id" : "4",
              "title" : "GraphQL"
            },
            {
              "id" : "5",
              "title" : "Scala"
            }
          ]
        }
      }
    """

    val compiled = FragmentMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("typed union fragment query") {
    val query = """
      query FragmentUnionTyping {
        user: user(id: "1") {
          favourite {
            __typename
            ...userFragment
            ...pageFragment
          }
        }
        page: user(id: "2") {
          favourite {
            __typename
            ...userFragment
            ...pageFragment
          }
        }
      }

      fragment userFragment on User {
        id
        name
      }

      fragment pageFragment on Page {
        id
        title
      }
    """

    val User = FragmentMapping.schema.ref("User")
    val Page = FragmentMapping.schema.ref("Page")

    val expected =
      Group(List(
        Select("user",
          Unique(
            Filter(Eql(FragmentMapping.UserType / "id", Const("1")),
              Select("favourite",
                Group(List(
                  Introspect(FragmentMapping.schema, Select("__typename")),
                  Narrow(
                    User,
                    Group(List(
                      Select("id"),
                      Select("name")
                    ))
                  ),
                  Narrow(
                    Page,
                    Group(List(
                      Select("id"),
                      Select("title")
                    ))
                   )
                ))
              )
            )
          )
        ),
        Select("user", Some("page"),
          Unique(
            Filter(Eql(FragmentMapping.PageType / "id", Const("2")),
              Select("favourite",
                Group(List(
                  Introspect(FragmentMapping.schema, Select("__typename")),
                  Narrow(
                    User,
                    Group(List(
                      Select("id"),
                      Select("name")
                    ))
                  ),
                  Narrow(
                    Page,
                    Group(List(
                      Select("id"),
                      Select("title")
                    ))
                  )
                ))
              )
            )
          )
        )
      ))

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "favourite" : {
              "__typename" : "User",
              "id" : "2",
              "name" : "Bob"
            }
          },
          "page" : {
            "favourite" : {
              "__typename" : "Page",
              "id" : "4",
              "title" : "GraphQL"
            }
          }
        }
      }
    """

    val compiled = FragmentMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("typed union nested fragment query") {
    val query = """
      query FragmentUnionTyping {
        user: user(id: "1") {
          favourite {
            __typename
            ...userOrPageFragment
          }
        }
      }

      fragment userFragment on User {
        id
        name
      }

      fragment pageFragment on Page {
        id
        title
      }

      fragment userOrPageFragment on UserOrPage {
        ...userFragment
        ...pageFragment
      }
    """

    val User = FragmentMapping.schema.ref("User")
    val Page = FragmentMapping.schema.ref("Page")
    val UserOrPage = FragmentMapping.schema.ref("UserOrPage")

    val expected =
      Group(List(
        Select("user",
          Unique(
            Filter(Eql(FragmentMapping.UserType / "id", Const("1")),
              Select("favourite",
                Group(List(
                  Introspect(FragmentMapping.schema, Select("__typename")),
                  Narrow(
                    User,
                    Group(List(
                      Select("id"),
                      Select("name")
                    ))
                  ),
                  Narrow(
                    Page,
                    Group(List(
                      Select("id"),
                      Select("title")
                    ))
                   )
                ))
              )
            )
          )
        ),
      ))

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "favourite" : {
              "__typename" : "User",
              "id" : "2",
              "name" : "Bob"
            }
          }
        }
      }
    """

    val compiled = FragmentMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("supertype fragment query (1)") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            ...profileFields
          }
        }
      }

      fragment profileFields on Profile {
        id
      }
    """

    val expected =
      Select("user",
        Unique(
          Filter(Eql(FragmentMapping.UserType / "id", Const("1")),
            Select("friends",
              Select("id")
            )
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "friends" : [
              {
                "id" : "2"
              },
              {
                "id" : "3"
              }
            ]
          }
        }
      }
    """

    val compiled = FragmentMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("supertype fragment query (2)") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            ... on Profile {
              id
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "user" : {
            "friends" : [
              {
                "id" : "2"
              },
              {
                "id" : "3"
              }
            ]
          }
        }
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("supertype fragment query (3)") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            ... on Profile {
              id
              ... on User {
                name
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "user" : {
            "friends" : [
              {
                "id" : "2",
                "name" : "Bob"
              },
              {
                "id" : "3",
                "name" : "Carol"
              }
            ]
          }
        }
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("supertype fragment query (4)") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            ... on Profile {
              id
              name
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "No field 'name' for type Profile"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("supertype fragment query (5)") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            ...profileFields
          }
        }
      }

      fragment profileFields on Profile {
        id
        name
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "No field 'name' for type Profile"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("interface query with supertype fragment containing subtype refinement") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            ... ProfileFields
          }
        }
      }

      fragment ProfileFields on Profile {
        id
        ... on User {
          name
        }
        ... on Page {
          title
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "user" : {
            "friends" : [
              {
                "id" : "2",
                "name" : "Bob"
              },
              {
                "id" : "3",
                "name" : "Carol"
              }
            ]
          }
        }
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("interface query with supertype fragment containing nested fragment spreads") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            ... ProfileFields
          }
        }
      }

      fragment ProfileFields on Profile {
        id
        ... UserFields
        ... PageFields
      }

      fragment UserFields on User {
        name
      }

      fragment PageFields on Page {
        title
      }
    """

    val expected = json"""
      {
        "data" : {
          "user" : {
            "friends" : [
              {
                "id" : "2",
                "name" : "Bob"
              },
              {
                "id" : "3",
                "name" : "Carol"
              }
            ]
          }
        }
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("interface query with nested inline fragments") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            ... on Profile {
              id
              ... on User {
                name
              }
              ... on Page {
                title
              }
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "user" : {
            "friends" : [
              {
                "id" : "2",
                "name" : "Bob"
              },
              {
                "id" : "3",
                "name" : "Carol"
              }
            ]
          }
        }
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("fragment defined") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            ...friendFields
          }
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Fragment 'friendFields' is undefined"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("fragment unused (1)") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            id
            name
            profilePic
          }
        }
      }

      fragment friendFields on User {
        id
        name
        profilePic
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Fragment 'friendFields' is unused"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("fragment unused (2)") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            id
            name
            profilePic
          }
        }
      }

      fragment friendFields on User {
        id
        name
        profilePic
      }
    """

    val expected = json"""
      {
        "data" : {
          "user" : {
            "friends" : [
              {
                "id" : "2",
                "name" : "Bob",
                "profilePic" : "B"
              },
              {
                "id" : "3",
                "name" : "Carol",
                "profilePic" : "C"
              }
            ]
          }
        }
      }
    """

    val res = FragmentMapping.compileAndRun(query, reportUnused = false)

    assertIO(res, expected)
  }

  test("fragment duplication") {
    val query = """
      query withFragments {
        user(id: 1) {
          ...userFields
        }
      }

      fragment userFields on User {
        name
      }

      fragment userFields on User {
        name
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Fragment 'userFields' is defined more than once"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }


  test("fragment recursion (1)") {
    val query = """
      query withFragments {
        user(id: 1) {
          ...userFields
        }
      }

      fragment userFields on User {
        name
        friends {
          ...userFields
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Fragment cycle starting from 'userFields'"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("fragment recursion (2)") {
    val query = """
      query withFragments {
        user(id: 1) {
          ...userFields
        }
      }

      fragment userFields on User {
        name
        favourite {
          ...pageFields
        }
      }

      fragment pageFields on Page {
        title
        likers {
          ...userFields
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Fragment cycle starting from 'userFields'"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("fragment recursion (3)") {
    val query = """
      query withFragments {
        user(id: 1) {
          ...userFields
        }
      }

      fragment userFields on User {
        name
        favourite {
          ...pageFields
        }
      }

      fragment pageFields on Page {
        title
        likers {
          ...userFields2
        }
      }

      fragment userFields2 on User {
        profilePic
        favourite {
          ...userFields
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Fragment cycle starting from 'userFields'"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("fragment recursion (4)") {
    val query = """
      query withFragments {
        user(id: 1) {
          ...userFields
        }
      }

      fragment pageFields on Page {
        title
        likers {
          ...userFields2
        }
      }

      fragment userFields2 on User {
        profilePic
        favourite {
          ...pageFields
        }
      }

      fragment userFields on User {
        name
        favourite {
          ...pageFields
        }
      }
    """

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Fragment cycle starting from 'pageFields'"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }
}

object FragmentData {
  sealed trait Profile {
    def id: String
  }

  case class User(id: String, name: String, profilePic: String, friendIds: List[String], favouriteId: Option[String]) extends Profile {
    def friends: List[User] =
      friendIds.flatMap(fid => profiles.collect { case u: User if u.id == fid => u })
    def mutualFriends: List[User] =
      friendIds.flatMap(fid => profiles.collect { case u: User if u.id == fid && u.friendIds.contains(id) => u })
    def favourite: Option[Profile] =
      favouriteId.flatMap(id => profiles.find(_.id == id))
  }

  case class Page(id: String, title: String, likerIds: List[String]) extends Profile {
    def likers: List[User] =
      likerIds.flatMap(lid => profiles.collect { case u: User if u.id == lid => u })
  }

  val profiles = List(
    User("1", "Alice", "A", List("2", "3"), Some("2")),
    User("2", "Bob", "B", List("1"), Some("4")),
    User("3", "Carol", "C", List("1", "2"), None),
    Page("4", "GraphQL", List("1", "3")),
    Page("5", "Scala", List("2"))
  )
}

object FragmentMapping extends ValueMapping[IO] {
  import FragmentData._

  val schema =
    schema"""
      type Query {
        user(id: ID!): User!
        profiles: [Profile!]!
      }
      type User implements Profile {
        id: String!
        name: String!
        profilePic: String!
        friends: [User!]!
        mutualFriends: [User!]!
        favourite: UserOrPage
      }
      type Page implements Profile {
        id: String!
        title: String!
        likers: [User!]!
      }
      union UserOrPage = User | Page
      interface Profile {
        id: String!
      }
    """

  val QueryType = schema.ref("Query")
  val ProfileType = schema.ref("Profile")
  val UserType = schema.ref("User")
  val PageType = schema.ref("Page")

  val typeMappings =
    List(
      ValueObjectMapping[Unit](
        tpe = QueryType,
        fieldMappings =
          List(
            ValueField("user", _ => profiles.collect { case u: User => u }),
            ValueField("profiles", _ => profiles)
          )
      ),
      ValueObjectMapping[Profile](
        tpe = ProfileType,
        fieldMappings =
          List(
            ValueField("id", _.id)
          )
      ),
      ValueObjectMapping[User](
        tpe = UserType,
        fieldMappings =
          List(
            ValueField("name", _.name),
            ValueField("profilePic", _.profilePic),
            ValueField("friends", _.friends),
            ValueField("mutualFriends", _.mutualFriends),
            ValueField("favourite", _.favourite),
          )
      ),
      ValueObjectMapping[Page](
        tpe = PageType,
        fieldMappings =
          List(
            ValueField("title", _.title),
            ValueField("likers", _.likers)
          )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "user", List(Binding("id", IDValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(FragmentMapping.UserType / "id", Const(id)), child)))
  }
}
