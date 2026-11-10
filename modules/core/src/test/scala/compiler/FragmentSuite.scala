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

package compiler

import cats.effect.IO
import cats.implicits._
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite
import utils.QueryLocations._

import grackle._
import grackle.Predicate._
import grackle.Query._
import grackle.QueryCompiler._
import grackle.Value._
import grackle.syntax._

final class FragmentSuite extends CatsEffectSuite {
  def runOperation(op: Result[Operation]): IO[List[Json]] = {
    val op0 = op.toOption.get
    FragmentMapping
      .interpreter
      .run(op0.query, op0.rootTpe, Env.empty)
      .evalMap(FragmentMapping.mkResponse)
      .compile
      .toList
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
      Select(
        "user",
        None,
        Unique(
          Filter(
            Eql(FragmentMapping.UserType / "id", Const("1")),
            Group(
              List(
                Select(
                  "friends",
                  None,
                  Group(
                    List(
                      Select("id", None, Empty, loc(14, 9)),
                      Select("name", None, Empty, loc(15, 9)),
                      Select("profilePic", None, Empty, loc(16, 9))
                    )),
                  loc(4, 11)
                ),
                Select(
                  "mutualFriends",
                  None,
                  Group(
                    List(
                      Select("id", None, Empty, loc(14, 9)),
                      Select("name", None, Empty, loc(15, 9)),
                      Select("profilePic", None, Empty, loc(16, 9))
                    )),
                  loc(7, 11)
                )
              ))
          )
        ),
        loc(3, 9)
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
      Select(
        "user",
        None,
        Unique(
          Filter(
            Eql(FragmentMapping.UserType / "id", Const("1")),
            Group(
              List(
                Select(
                  "friends",
                  None,
                  Group(
                    List(
                      Select("id", None, Empty, loc(14, 9)),
                      Select("name", None, Empty, loc(15, 9)),
                      Select("profilePic", None, Empty, loc(20, 9))
                    )),
                  loc(4, 11)
                ),
                Select(
                  "mutualFriends",
                  None,
                  Group(
                    List(
                      Select("id", None, Empty, loc(14, 9)),
                      Select("name", None, Empty, loc(15, 9)),
                      Select("profilePic", None, Empty, loc(20, 9))
                    )),
                  loc(7, 11)
                )
              ))
          )
        ),
        loc(3, 9)
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
      Select(
        "user",
        None,
        Unique(
          Filter(
            Eql(FragmentMapping.UserType / "id", Const("1")),
            Group(
              List(
                Select(
                  "friends",
                  None,
                  Group(
                    List(
                      Select("id", None, Empty, loc(14, 9)),
                      Select("name", None, Empty, loc(19, 9)),
                      Select("profilePic", None, Empty, loc(24, 9))
                    )),
                  loc(4, 11)
                ),
                Select(
                  "mutualFriends",
                  None,
                  Group(
                    List(
                      Select("id", None, Empty, loc(14, 9)),
                      Select("name", None, Empty, loc(19, 9)),
                      Select("profilePic", None, Empty, loc(24, 9))
                    )),
                  loc(7, 11)
                )
              ))
          )
        ),
        loc(3, 9)
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
      Select(
        "profiles",
        None,
        Group(
          List(
            Select("id", None, Empty, loc(4, 11)),
            Introspect(FragmentMapping.schema, Select("__typename", None, Empty, loc(5, 11))),
            Narrow(User, Select("name", None, Empty, loc(12, 9))),
            Narrow(Page, Select("title", None, Empty, loc(16, 9)))
          )),
        loc(3, 9)
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
      Select(
        "profiles",
        None,
        Group(
          List(
            Select("id", None, Empty, loc(4, 11)),
            Narrow(User, Select("name", None, Empty, loc(6, 13))),
            Narrow(Page, Select("title", None, Empty, loc(9, 13)))
          )),
        loc(3, 9)
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
      Group(
        List(
          Select(
            "user",
            None,
            Unique(
              Filter(
                Eql(FragmentMapping.UserType / "id", Const("1")),
                Select(
                  "favourite",
                  None,
                  Group(
                    List(
                      Introspect(
                        FragmentMapping.schema,
                        Select("__typename", None, Empty, loc(5, 13))),
                      Narrow(
                        User,
                        Group(List(
                          Select("id", None, Empty, loc(20, 9)),
                          Select("name", None, Empty, loc(21, 9))
                        ))
                      ),
                      Narrow(
                        Page,
                        Group(List(
                          Select("id", None, Empty, loc(25, 9)),
                          Select("title", None, Empty, loc(26, 9))
                        ))
                      )
                    )),
                  loc(4, 11)
                )
              )
            ),
            loc(3, 9)
          ),
          Select(
            "user",
            Some("page"),
            Unique(
              Filter(
                Eql(FragmentMapping.PageType / "id", Const("2")),
                Select(
                  "favourite",
                  None,
                  Group(
                    List(
                      Introspect(
                        FragmentMapping.schema,
                        Select("__typename", None, Empty, loc(12, 13))),
                      Narrow(
                        User,
                        Group(List(
                          Select("id", None, Empty, loc(20, 9)),
                          Select("name", None, Empty, loc(21, 9))
                        ))
                      ),
                      Narrow(
                        Page,
                        Group(List(
                          Select("id", None, Empty, loc(25, 9)),
                          Select("title", None, Empty, loc(26, 9))
                        ))
                      )
                    )),
                  loc(11, 11)
                )
              )
            ),
            loc(10, 9)
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

    val expected =
      Select(
        "user",
        None,
        Unique(
          Filter(
            Eql(FragmentMapping.UserType / "id", Const("1")),
            Select(
              "favourite",
              None,
              Group(
                List(
                  Introspect(
                    FragmentMapping.schema,
                    Select("__typename", None, Empty, loc(5, 13))),
                  Narrow(
                    User,
                    Group(
                      List(
                        Select("id", None, Empty, loc(12, 9)),
                        Select("name", None, Empty, loc(13, 9))
                      ))
                  ),
                  Narrow(
                    Page,
                    Group(
                      List(
                        Select("id", None, Empty, loc(17, 9)),
                        Select("title", None, Empty, loc(18, 9))
                      ))
                  )
                )),
              loc(4, 11)
            )
          )
        ),
        loc(3, 9)
      )

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
      Select(
        "user",
        None,
        Unique(
          Filter(
            Eql(FragmentMapping.UserType / "id", Const("1")),
            Select("friends", None, Select("id", None, Empty, loc(11, 9)), loc(4, 11))
          )
        ),
        loc(3, 9)
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

  test("fragment shared by multiple paths (not a cycle)") {
    val query = """
      query withFragments {
        user(id: 1) {
          ...userFields
        }
      }

      fragment userFields on User {
        friends {
          ...nameFields
        }
        mutualFriends {
          ...mutualFriendFields
        }
      }

      fragment mutualFriendFields on User {
        ...nameFields
      }

      fragment nameFields on User {
        id
        name
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
            ],
            "mutualFriends" : [
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

  test("deeply nested fragment expansion") {
    val query =
      "query Q { user(id: 1) { ...G1 } }\n" +
        (1 until 10000)
          .map(i => s"fragment G$i on User { friends { ...G${i + 1} } }\n")
          .mkString +
        "fragment G10000 on User { id }\n"

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Operation 'Q', with fragment spreads expanded, exceeds the maximum selection depth of 100"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("deeply nested fragment spreads") {
    val query =
      "query Q { ...G1 }\n" +
        (1 until 10000).map(i => s"fragment G$i on Query { ...G${i + 1} }\n").mkString +
        "fragment G10000 on Query { user(id: 1) { id } }\n"

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Operation 'Q', with fragment spreads expanded, exceeds the maximum fragment spread depth of 100"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("list argument field") {
    val query =
      """query { user(id: 1) { yes: hasFriendIn(ids: ["4", "3"]) no: hasFriendIn(ids: ["4"]) } }"""

    val expected = json"""
      {
        "data" : {
          "user" : {
            "yes" : true,
            "no" : false
          }
        }
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("fragment expansion with a large variable") {
    // 2,048 expanded references to $x, each replaced by its 101-node value
    val query =
      "query Q($x: [ID!]!) { user(id: 1) { ...G1 } }\n" +
        (1 until 12)
          .map(i =>
            s"fragment G$i on User { a: friends { ...G${i + 1} } b: friends { ...G${i + 1} } }\n")
          .mkString +
        "fragment G12 on User { hasFriendIn(ids: $x) }\n"

    val vars =
      Json.obj("x" -> Json.fromValues((1 to 100).map(i => Json.fromString(i.toString))))

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Operation 'Q', with fragment spreads expanded, exceeds the maximum selection size of 100000"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query, untypedVars = Some(vars))

    assertIO(res, expected)
  }

  test("exponential fragment expansion") {
    val query =
      "query Q { user(id: 1) { ...G1 } }\n" +
        (1 until 40)
          .map(i =>
            s"fragment G$i on User { a: friends { ...G${i + 1} } b: friends { ...G${i + 1} } }\n")
          .mkString +
        "fragment G40 on User { id }\n"

    val expected = json"""
      {
        "errors" : [
          {
            "message" : "Operation 'Q', with fragment spreads expanded, exceeds the maximum selection size of 100000"
          }
        ]
      }
    """

    val res = FragmentMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  def limitedCompiler(
      maxSelectionDepth: Int = 100,
      maxSelectionSize: Int = 100000): QueryCompiler = {
    val config = GraphQLParser
      .defaultConfig
      .copy(maxSelectionDepth = maxSelectionDepth, maxSelectionSize = maxSelectionSize)

    new QueryCompiler(
      QueryParser(GraphQLParser(config)),
      FragmentMapping.schema,
      FragmentMapping.compilerPhases,
      config)
  }

  def problems(res: Result[Operation]): List[String] =
    res.toProblems.toList.map(_.message)

  test("compileOperation validates its operation") {
    // Compiles the first operation of `text`
    def compileFirst(text: String): Result[Operation] =
      FragmentMapping.queryParser.parseText(text).flatMap {
        case (ops, frags) => FragmentMapping.compiler.compileOperation(ops.head, None, frags)
      }

    assertEquals(
      problems(compileFirst("query Q { user(id: $x) { id } }")),
      List("Variable 'x' is undefined"))
    assertEquals(
      problems(
        compileFirst(
          "query Q { user(id: 1) { ...F } } fragment F on User { friends { ...F } }")),
      List("Fragment cycle starting from 'F'"))
    assertEquals(
      problems(compileFirst("query Q { user(id: 1) { a: name a: id } }")),
      List("Cannot merge fields with alias 'a' and names 'name', 'id'"))
    // A fragment which only another operation uses isn't reported as unused
    assert(compileFirst(
      "query Q { user(id: 1) { id } } query R { user(id: 1) { ...F } } fragment F on User { name }").hasValue)
    // Nor are unused variables
    assert(compileFirst("query Q($x: ID) { user(id: 1) { id } }").hasValue)
  }

  test("expanded selection depth") {
    // The selections of a fragment are merged into the enclosing selection set, so the selection
    // sets are the operation's, user's and friends': three levels
    def mkQuery(fields: String): String =
      s"query Q { user(id: 1) { ...F } } fragment F on User { friends { ...G } } fragment G on User { $fields }"

    val compiler = limitedCompiler(maxSelectionDepth = 3)

    assert(compiler.compile(mkQuery("name")).hasValue)
    assertEquals(
      problems(compiler.compile(mkQuery("friends { name }"))),
      List(
        "Operation 'Q', with fragment spreads expanded, exceeds the maximum selection depth of 3")
    )
  }

  test("expanded fragment spread depth") {
    def mkQuery(n: Int): String =
      "query Q { ...F1 } " + (1 until n)
        .map(i => s"fragment F$i on Query { ...F${i + 1} } ")
        .mkString +
        s"fragment F$n on Query { user(id: 1) { id } }"

    val compiler = limitedCompiler(maxSelectionDepth = 3)

    assert(compiler.compile(mkQuery(3)).hasValue)
    assertEquals(
      problems(compiler.compile(mkQuery(4))),
      List(
        "Operation 'Q', with fragment spreads expanded, exceeds the maximum fragment spread depth of 3"))
  }

  test("unused fragments are not measured") {
    // Each fragment is within the parser's literal depth limit, but F exceeds the limit once
    // expanded
    val frags =
      "fragment F on User { friends { ...G } } fragment G on User { friends { name } }"

    val compiler = limitedCompiler(maxSelectionDepth = 3)

    assert(
      compiler
        .compile(s"query Q { user(id: 1) { name } } $frags", reportUnused = false)
        .hasValue)
    assertEquals(
      problems(compiler.compile(s"query Q { user(id: 1) { ...F } } $frags")),
      List(
        "Operation 'Q', with fragment spreads expanded, exceeds the maximum selection depth of 3")
    )
  }

  test("expanded selection size") {
    // user, id: 1, then friends, ...G, and G's selections for each of a and b
    def mkQuery(fields: String): String =
      s"query Q { user(id: 1) { a: friends { ...G } b: friends { ...G } } } fragment G on User { $fields }"

    val compiler = limitedCompiler(maxSelectionSize = 8)
    val expected =
      List(
        "Operation 'Q', with fragment spreads expanded, exceeds the maximum selection size of 8")

    assert(compiler.compile(mkQuery("name")).hasValue)
    assertEquals(problems(compiler.compile(mkQuery("name id"))), expected)
    // A directive and its argument count, as well as the selection
    assertEquals(problems(compiler.compile(mkQuery("name @include(if: true)"))), expected)
  }

  test("expanded selection size includes argument values") {
    val query =
      s"query Q { user(id: 1) { hasFriendIn(ids: [${List.fill(10)("\"1\"").mkString(", ")}]) } }"

    assert(limitedCompiler(maxSelectionSize = 100).compile(query).hasValue)

    assertEquals(
      problems(limitedCompiler(maxSelectionSize = 8).compile(query)),
      List(
        "Operation 'Q', with fragment spreads expanded, exceeds the maximum selection size of 8")
    )
  }

  test("selection size without fragments") {
    val compiler = limitedCompiler(maxSelectionSize = 4)

    assert(compiler.compile("query { user(id: 1) { id name } }").hasValue)
    assertEquals(
      problems(compiler.compile("query { user(id: 1) { id name profilePic } }")),
      List(
        "Anonymous operation, with fragment spreads expanded, exceeds the maximum selection size of 4")
    )
  }

  test("fragment chain at the depth limit") {
    // Each link in the chain contributes a selection set and a fragment spread. The limit is
    // kept small because elaborating a long fragment chain can exhaust the smaller stack of
    // Scala.js.
    def mkQuery(n: Int): String =
      "query Q { user(id: 1) { ...G1 } }\n" +
        (1 until n).map(i => s"fragment G$i on User { friends { ...G${i + 1} } }\n").mkString +
        s"fragment G$n on User { id }\n"

    val compiler = limitedCompiler(maxSelectionDepth = 20)

    assert(compiler.compile(mkQuery(19)).hasValue)
    assertEquals(
      problems(compiler.compile(mkQuery(20))),
      List(
        "Operation 'Q', with fragment spreads expanded, exceeds the maximum selection depth of 20")
    )
  }

  test(
    "long chains of unused or cyclic fragments are validated without overflowing the stack") {
    def chain(n: Int, last: String): String =
      (1 until n).map(i => s"fragment G$i on User { friends { ...G${i + 1} } }\n").mkString +
        s"fragment G$n on User { $last }\n"

    val unused = "query Q { user(id: 1) { id } }\n" + chain(10000, "id")
    assert(FragmentMapping.compiler.compile(unused, reportUnused = false).hasValue)

    val cyclic = "query Q { user(id: 1) { ...G1 } }\n" + chain(10000, "friends { ...G1 }")
    val cyclicProblems = problems(FragmentMapping.compiler.compile(cyclic))
    assertEquals(cyclicProblems.size, 1)
    assert(cyclicProblems.head.startsWith("Fragment cycle starting from"), cyclicProblems.head)
  }
}

object FragmentData {
  sealed trait Profile {
    def id: String
  }

  case class User(
      id: String,
      name: String,
      profilePic: String,
      friendIds: List[String],
      favouriteId: Option[String])
      extends Profile {
    def friends: List[User] =
      friendIds.flatMap(fid => profiles.collect { case u: User if u.id == fid => u })
    def mutualFriends: List[User] =
      friendIds.flatMap(fid =>
        profiles.collect { case u: User if u.id == fid && u.friendIds.contains(id) => u })
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
        hasFriendIn(ids: [ID!]!): Boolean!
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
        fieldMappings = List(
          ValueField("user", _ => profiles.collect { case u: User => u }),
          ValueField("profiles", _ => profiles)
        )
      ),
      ValueObjectMapping[Profile](
        tpe = ProfileType,
        fieldMappings = List(
          ValueField("id", _.id)
        )
      ),
      ValueObjectMapping[User](
        tpe = UserType,
        fieldMappings = List(
          ValueField("name", _.name),
          ValueField("profilePic", _.profilePic),
          ValueField("friends", _.friends),
          ValueField("mutualFriends", _.mutualFriends),
          ValueField("favourite", _.favourite),
          CursorField("hasFriendIn", hasFriendIn)
        )
      ),
      ValueObjectMapping[Page](
        tpe = PageType,
        fieldMappings = List(
          ValueField("title", _.title),
          ValueField("likers", _.likers)
        )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "user", List(Binding("id", IDValue(id)))) =>
      Elab.transformChild(child =>
        Unique(Filter(Eql(FragmentMapping.UserType / "id", Const(id)), child)))
    case (UserType, "hasFriendIn", List(Binding("ids", ListValue(ids)))) =>
      Elab.env("ids" -> ids.collect { case IDValue(id) => id })
  }

  def hasFriendIn(c: Cursor): Result[Boolean] =
    for {
      user <- c.as[User]
      ids <- c.envR[List[String]]("ids")
    } yield ids.exists(user.friendIds.contains)
}
