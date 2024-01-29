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
import PathTerm.UniquePath
import Query._
import Predicate._, Value._
import QueryCompiler._

final class FieldMergeSuite extends CatsEffectSuite {
  def runOperation(op: Result[Operation]): IO[List[Json]] = {
    val op0 = op.toOption.get
    FieldMergeMapping.interpreter.run(op0.query, op0.rootTpe, Env.empty).evalMap(FieldMergeMapping.mkResponse).compile.toList
  }

  test("merge fields") {
    val query = """
      query {
        user(id: 1) {
          name
          profilePic
          name
          id
          foo:name
          foo:name
          bar:name

          friends {
            name
          }

          friends {
            profilePic
          }

          friends {
            id
          }

          baz:friends {
            name
            quux:name
          }

          baz:friends {
            name
            profilePic
          }
          baz:friends {
            quux:name
            id
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(Eql(UniquePath(List("id")), Const("1")),
            Group(List(
              Select("name", None, Empty),
              Select("profilePic", None, Empty),
              Select("id", None, Empty),
              Select("name", Some("foo"), Empty),
              Select("name", Some("bar"), Empty),
              Select("friends", None,
                Group(List(
                  Select("name", None, Empty),
                  Select("profilePic", None, Empty),
                  Select("id", None, Empty)
                ))
              ),
              Select("friends", Some("baz"),
                Group(List(
                  Select("name", None, Empty),
                  Select("name", Some("quux"), Empty),
                  Select("profilePic", None, Empty),
                  Select("id", None, Empty)
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
            "name" : "Alice",
            "profilePic" : "A",
            "id" : "1",
            "foo" : "Alice",
            "bar" : "Alice",
            "friends" : [
              {
                "name" : "Bob",
                "profilePic" : "B",
                "id" : "2"
              },
              {
                "name" : "Carol",
                "profilePic" : "C",
                "id" : "3"
              }
            ],
            "baz" : [
              {
                "name" : "Bob",
                "quux" : "Bob",
                "profilePic" : "B",
                "id" : "2"
              },
              {
                "name" : "Carol",
                "quux" : "Carol",
                "profilePic" : "C",
                "id" : "3"
              }
            ]
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("mergeable alias") {
    val query = """
      query {
        user(id: 1) {
          profilePic:name
          name:profilePic
          profilePic:name
          name:profilePic
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(Eql(UniquePath(List("id")), Const("1")),
            Group(List(
              Select("name", Some("profilePic"), Empty),
              Select("profilePic", Some("name"), Empty)
            ))
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "profilePic" : "Alice",
            "name" : "A"
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("unmergeable alias (1)") {
    val query = """
      query {
        user(id: 1) {
          name
          name:profilePic
        }
      }
    """

    val expected =
      "Cannot merge fields with alias 'name' and names 'name', 'profilePic'"

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }

  test("unmergeable alias (2)") {
    val query = """
      query {
        user(id: 1) {
          foo:name
          foo:profilePic
        }
      }
    """

    val expected =
      "Cannot merge fields with alias 'foo' and names 'name', 'profilePic'"

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }

  test("fields with arguments (1)") {
    val query = """
      query {
        user(id: 1) {
          name
        }
        user(id: 1) {
          profilePic
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(Eql(UniquePath(List("id")), Const("1")),
            Group(List(
              Select("name", None, Empty),
              Select("profilePic", None, Empty)
            ))
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "name" : "Alice",
            "profilePic" : "A"
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("fields with arguments (2)") {
    val query = """
      query {
        user(id: 1) {
          name
        }
        user(id: 2) {
          profilePic
        }
      }
    """

    val expected =
      "Cannot merge fields named 'user' with different arguments"

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }

  test("fields with arguments (3)") {
    val query = """
      query {
        user(id: 1) {
          name
        }
        foo:user(id: 1) {
          profilePic
        }
      }
    """

    val expected =
      Group(List(
        Select("user", None,
          Unique(
            Filter(Eql(UniquePath(List("id")), Const("1")),
              Select("name", None, Empty)
            )
          )
        ),
        Select("user", Some("foo"),
          Unique(
            Filter(Eql(UniquePath(List("id")), Const("1")),
              Select("profilePic", None, Empty)
            )
          )
        )
      ))

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "name" : "Alice"
          },
          "foo" : {
            "profilePic" : "A"
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("fields with arguments (4)") {
    val query = """
      query {
        user(id: 1) {
          name
        }
        foo:user(id: 2) {
          profilePic
        }
      }
    """

    val expected =
      Group(List(
        Select("user", None,
          Unique(
            Filter(Eql(UniquePath(List("id")), Const("1")),
              Select("name", None, Empty)
            )
          )
        ),
        Select("user", Some("foo"),
          Unique(
            Filter(Eql(UniquePath(List("id")), Const("2")),
              Select("profilePic", None, Empty)
            )
          )
        )
      ))

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "name" : "Alice"
          },
          "foo" : {
            "profilePic" : "B"
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("fields with arguments (5)") {
    val query = """
      query ($id: ID!) {
        user(id: $id) {
          name
        }
        user(id: $id) {
          profilePic
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(Eql(UniquePath(List("id")), Const("1")),
            Group(List(
              Select("name", None, Empty),
              Select("profilePic", None, Empty)
            ))
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "name" : "Alice",
            "profilePic" : "A"
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query, untypedVars = Some(json"""{ "id": "1" }"""))

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("fields with arguments (6)") {
    val query = """
      query ($id1: ID!, $id2: ID!) {
        user(id: $id1) {
          name
        }
        user(id: $id2) {
          profilePic
        }
      }
    """

    val expected =
      "Cannot merge fields named 'user' with different arguments"

    val compiled = FieldMergeMapping.compiler.compile(query, untypedVars = Some(json"""{ "id1": "1", "id2": "2" }"""))

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }

  test("fields with arguments (7)") {
    val query = """
      query ($id: ID!) {
        user(id: $id) {
          name
        }
        user(id: 1) {
          profilePic
        }
      }
    """

    val expected =
      "Cannot merge fields named 'user' with different arguments"

    val compiled = FieldMergeMapping.compiler.compile(query, untypedVars = Some(json"""{ "id1": "1" }"""))

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }


  test("fields with skip (1)") {
    val query = """
      query {
        user(id: 1) {
          friends {
            name
          }
          friends @skip(if: true) {
            profilePic
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(Eql(UniquePath(List("id")), Const("1")),
            Select("friends", None,
              Select("name", None, Empty)
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
                "name" : "Bob"
              },
              {
                "name" : "Carol"
              }
            ]
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("fields with skip (2)") {
    val query = """
      query {
        user(id: 1) {
          friends @skip(if: false) {
            name
          }
          friends @skip(if: true) {
            profilePic
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(Eql(UniquePath(List("id")), Const("1")),
            Select("friends", None,
              Select("name", None, Empty)
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
                "name" : "Bob"
              },
              {
                "name" : "Carol"
              }
            ]
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("fields with skip (3)") {
    val query = """
      query {
        user(id: 1) {
          friends {
            name
          }
          friends {
            profilePic @skip(if: true)
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(Eql(UniquePath(List("id")), Const("1")),
            Select("friends", None,
              Select("name", None, Empty)
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
                "name" : "Bob"
              },
              {
                "name" : "Carol"
              }
            ]
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("fields with skip (4)") {
    val query = """
      query {
        user(id: 1) {
          friends {
            name
          }
        }
        user(id: 2) @skip(if: true) {
          friends {
            name
          }
        }
      }
    """

    val expected =
      "Cannot merge fields named 'user' with different arguments"

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }

  test("fields with skip (5)") {
    val query = """
      query {
        user(id: 1) {
          friends @skip(if: false) {
            name
          }
          friends @skip(if: false) {
            profilePic
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(Eql(UniquePath(List("id")), Const("1")),
            Select("friends", None,
              Group(List(
                Select("name", None, Empty),
                Select("profilePic", None, Empty)
              ))
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
                "name" : "Bob",
                "profilePic" : "B"
              },
              {
                "name" : "Carol",
                "profilePic" : "C"
              }
            ]
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("fields with variants (1)") {
    val query = """
      query {
        user(id: 1) {
          favourite {
            ... on User {
              label:name
            }
            ... on Page {
              label:title
            }
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(
            Eql(UniquePath(List("id")), Const("1")),
            Select("favourite", None,
              Group(List(
                Narrow(FieldMergeMapping.UserType, Select("name", Some("label"), Empty)),
                Narrow(FieldMergeMapping.PageType, Select("title", Some("label"), Empty))
              ))
            )
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "favourite" : {
              "label" : "Bob"
            }
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("fields with variants (2)") {
    val query = """
      query {
        user(id: 1) {
          favourite {
            ... on User {
              label:age
            }
            ... on Page {
              label:title
            }
          }
        }
      }
    """

    val expected =
      "Cannot merge fields named 'label' of distinct leaf types Int, String"

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }

  test("fields with variants (3)") {
    val query = """
      query {
        user(id: 1) {
          favourite {
            ... on User {
              likers:friends {
                name
              }
            }
            ... on Page {
              likers {
                name
              }
            }
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(
            Eql(UniquePath(List("id")), Const("1")),
            Select("favourite", None,
              Group(List(
                Narrow(FieldMergeMapping.UserType,
                  Select("friends", Some("likers"),
                    Select("name", None, Empty)
                  )
                ),
                Narrow(FieldMergeMapping.PageType,
                  Select("likers", None,
                    Select("name", None, Empty)
                  )
                )
              ))
            )
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "favourite" : {
              "likers" : [
                {
                  "name" : "Alice"
                }
              ]
            }
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("fields with variants (4)") {
    val query = """
      query {
        user(id: 1) {
          favourite {
            ... on User {
              friends {
                name
              }
            }
            ... on Page {
              friends:creator {
                name
              }
            }
          }
        }
      }
    """

    val expected =
      "Cannot merge fields named 'friends' of both list and non-list types"

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }

  test("fields with variants (5)") {
    val query = """
      query {
        user(id: 1) {
          favourite {
            ... on User {
              favourite {
                ... on User {
                  name
                }
              }
            }
            ... on Page {
              favourite:creator {
                name
              }
            }
          }
        }
      }
    """

    val expected =
      "Cannot merge fields named 'favourite' of both nullable and non-nullable types"

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }

  test("fields with variants (6)") {
    val query = """
      query {
        user(id: 1) {
          favourite {
            ... on User {
              label:name
            }
            ... on Page {
              label:creator {
                id
              }
            }
          }
        }
      }
    """

    val expected =
      "Cannot merge fields named 'label' of leaf types String and non-leaf types User"

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }

  test("fields with directives (1)") {
    val query = """
      query {
        user(id: 1) {
          name @foo
          name @foo
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(
            Eql(UniquePath(List("id")), Const("1")),
            Select("name", None, Empty)
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "name" : "Alice"
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("fields with directives (2)") {
    val query = """
      query {
        user(id: 1) {
          name
          name @foo
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(
            Eql(UniquePath(List("id")), Const("1")),
            Select("name", None, Empty)
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "name" : "Alice"
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("fields with directives (3)") {
    val query = """
      query {
        user(id: 1) {
          name @foo
          name @bar
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(
            Eql(UniquePath(List("id")), Const("1")),
            Select("name", None, Empty)
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "name" : "Alice"
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("inline fragments (1)") {
    val query = """
      query {
        ... on Query {
          user(id: 1) {
            name
          }
        }
        ... on Query {
          user(id: 1) {
            profilePic
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(Eql(UniquePath(List("id")), Const("1")),
            Group(List(
              Select("name", None, Empty),
              Select("profilePic", None, Empty)
            ))
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "name" : "Alice",
            "profilePic" : "A"
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("inline fragments (2)") {
    val query = """
      query {
        user(id: 1) {
          ... on User {
            name
          }
        }
        user(id: 1) {
          ... on User {
            profilePic
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(Eql(UniquePath(List("id")), Const("1")),
            Group(List(
              Select("name", None, Empty),
              Select("profilePic", None, Empty)
            ))
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "name" : "Alice",
            "profilePic" : "A"
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("inline fragments (3)") {
    val query = """
      query {
        user(id: 1) {
          favourite {
            ... on User {
              name
            }
          }
        }
        user(id: 1) {
          favourite {
            ... on User {
              profilePic
            }
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(
            Eql(UniquePath(List("id")), Const("1")),
            Select("favourite", None,
              Narrow(FieldMergeMapping.UserType,
                Group(List(
                  Select("name", None, Empty),
                  Select("profilePic", None, Empty)
                ))
              )
            )
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "favourite" : {
              "name" : "Bob",
              "profilePic" : "B"
            }
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("inline fragments (4)") {
    val query = """
      query {
        user(id: 1) {
          favourite {
            ... on User {
              name
            }
          }
        }
        user(id: 1) {
          favourite {
            ... on User {
              name:profilePic
            }
          }
        }
      }
    """

    val expected =
      "Cannot merge fields with alias 'name' and names 'name', 'profilePic'"

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }

  test("inline fragments (5)") {
    val query = """
      query {
        user(id: 1) {
          ... on User {
            favourite {
              ... on User {
                name
              }
              ... on Profile {
                id
              }
            }
          }
        }
        user(id: 1) {
          ... on User {
            favourite {
              ... on User {
                id
                name
              }
            }
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(
            Eql(UniquePath(List("id")), Const("1")),
            Select("favourite", None,
              Group(List(
                Narrow(FieldMergeMapping.UserType,
                  Group(List(
                    Select("name", None, Empty),
                    Select("id", None, Empty)
                  ))
                ),
                Narrow(FieldMergeMapping.ProfileType, Select("id", None, Empty))
              ))
            )
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "favourite" : {
              "name" : "Bob",
              "id" : "2"
            }
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("inline fragments (6)") {
    val query = """
      query {
        user(id: 1) {
          favourite {
            ... on User {
              id:name
            }
            ... on Profile {
              id
            }
          }
        }
      }
    """

    val expected =
      "Cannot merge fields with alias 'id' and names 'name', 'id'"

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }

  test("inline fragments (7)") {
    val query = """
      query {
        user(id: 1) {
          friends {
            ... on User {
              friends {
                name
              }
            }
            ... on Profile {
              ... on User {
                friends {
                  id
                }
              }
            }
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(
            Eql(UniquePath(List("id")), Const("1")),
            Select("friends", None,
              Group(List(
                Select("friends", None,
                  Select("name", None, Empty)
                ),
                Narrow(FieldMergeMapping.UserType,
                  Select("friends", None,
                    Select("id", None, Empty)
                  )
                )
              ))
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
                "friends" : [
                  {
                    "name" : "Alice",
                    "id" : "1"
                  }
                ]
              },
              {
                "friends" : [
                  {
                    "name" : "Alice",
                    "id" : "1"
                  },
                  {
                    "name" : "Bob",
                    "id" : "2"
                  }
                ]
              }
            ]
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("inline fragments (8)") {
    val query = """
      query {
        user(id: 1) {
          friends {
            ... on Profile {
              id
              ... on User {
                friends {
                  name
                }
              }
            }
          }
        }
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(
            Eql(UniquePath(List("id")), Const("1")),
            Select("friends", None,
              Group(List(
                Select("id", None, Empty),
                Narrow(FieldMergeMapping.UserType,
                  Select("friends", None,
                    Select("name", None, Empty)
                  )
                )
              ))
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
                "id" : "2",
                "friends" : [
                  {
                    "name" : "Alice"
                  }
                ]
              },
              {
                "id" : "3",
                "friends" : [
                  {
                    "name" : "Alice"
                  },
                  {
                    "name" : "Bob"
                  }
                ]
              }
            ]
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("inline fragments (9)") {
    val query = """
      query {
        user(id: 1) {
          friends {
            ... on Profile {
              id
              ... on User {
                id:name
              }
            }
          }
        }
      }
    """

    val expected =
      "Cannot merge fields with alias 'id' and names 'id', 'name'"

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.failure(expected))
  }

  test("merge across fragments") {
    val query = """
      query {
        user(id: 1) {
          friends {
            name
          }
          ...userFields
        }
      }

      fragment userFields on User {
        friends {
          profilePic
        }
        id
      }
    """

    val expected =
      Select("user", None,
        Unique(
          Filter(
            Eql(UniquePath(List("id")), Const("1")),
            Group(
              List(
                Select("friends", None,
                  Group(
                    List(
                      Select("name", None, Empty),
                      Select("profilePic", None, Empty)
                    )
                  )
                ),
                Select("id", None, Empty)
              )
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
                "name" : "Bob",
                "profilePic" : "B"
              },
              {
                "name" : "Carol",
                "profilePic" : "C"
              }
            ],
            "id" : "1"
          }
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }

  test("merge across top level fragments") {
    val query = """
      query {
        user(id: 1) {
          name
        }

        ...queryFields
      }

      fragment queryFields on Query {
        user(id: 1) {
          profilePic
        }

        profiles {
          id
        }
      }
    """

    val expected =
      Group(
        List(
          Select("user", None,
            Unique(
              Filter(Eql(UniquePath(List("id")), Const("1")),
                Group(
                  List(
                    Select("name", None, Empty),
                    Select("profilePic", None, Empty)
                  )
                )
              )
            )
          ),
          Select("profiles", None,
            Select("id", None, Empty)
          )
        )
      )

    val expectedResult = json"""
      {
        "data" : {
          "user" : {
            "name" : "Alice",
            "profilePic" : "A"
          },
          "profiles" : [
            {
              "id" : "1"
            },
            {
              "id" : "2"
            },
            {
              "id" : "3"
            },
            {
              "id" : "4"
            },
            {
              "id" : "5"
            }
          ]
        }
      }
    """

    val compiled = FieldMergeMapping.compiler.compile(query)

    assertEquals(compiled.map(_.query), Result.Success(expected))

    val res = runOperation(compiled)

    assertIO(res, List(expectedResult))
  }
}

object FieldMergeData {
  sealed trait Profile {
    def id: String
  }

  case class User(id: String, name: String, age: Int, profilePic: String, friendIds: List[String], favouriteId: Option[String]) extends Profile {
    def friends: List[User] =
      friendIds.flatMap(fid => profiles.collect { case u: User if u.id == fid => u })
    def mutualFriends: List[User] =
      friendIds.flatMap(fid => profiles.collect { case u: User if u.id == fid && u.friendIds.contains(id) => u })
    def favourite: Option[Profile] =
      favouriteId.flatMap(id => profiles.find(_.id == id))
  }

  case class Page(id: String, title: String, likerIds: List[String], creatorId: String) extends Profile {
    def likers: List[User] =
      likerIds.flatMap(lid => profiles.collect { case u: User if u.id == lid => u })
    def creator: User =
      profiles.collect { case u: User if u.id == creatorId => u }.head
  }

  val profiles = List(
    User("1", "Alice", 23, "A", List("2", "3"), Some("2")),
    User("2", "Bob", 24, "B", List("1"), Some("4")),
    User("3", "Carol", 25, "C", List("1", "2"), None),
    Page("4", "GraphQL", List("1", "3"), "2"),
    Page("5", "Scala", List("2"), "1")
  )
}

object FieldMergeMapping extends ValueMapping[IO] {
  import FieldMergeData._

  val schema =
    schema"""
      type Query {
        user(id: ID!): User!
        profiles: [Profile!]!
      }
      type User implements Profile {
        id: String!
        name: String!
        age: Int!
        profilePic: String!
        friends: [User!]!
        mutualFriends: [User!]!
        favourite: UserOrPage
      }
      type Page implements Profile {
        id: String!
        title: String!
        likers: [User!]!
        creator: User!
      }
      union UserOrPage = User | Page
      interface Profile {
        id: String!
      }

      directive @foo on FIELD
      directive @bar on FIELD
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
            ValueField("age", _.age),
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
            ValueField("likers", _.likers),
            ValueField("creator", _.creator)
          )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "user", List(Binding("id", IDValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(FieldMergeMapping.UserType / "id", Const(id)), child)))
  }
}
