// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.Id
import cats.data.Ior
import cats.implicits._
import cats.tests.CatsSuite
import io.circe.literal.JsonStringContext

import edu.gemini.grackle._
import Query._, Predicate._, Value._
import QueryCompiler._

final class FragmentSuite extends CatsSuite {
  test("simple fragment query") {
    val text = """
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
      Select("user", Nil,
        Unique(FieldEquals("id", "1"),
          Group(List(
            Select("friends", Nil,
              Group(List(
                Select("id", Nil, Empty),
                Select("name", Nil, Empty),
                Select("profilePic", Nil, Empty)
              ))
            ),
            Select("mutualFriends", Nil,
              Group(List(
                Select("id", Nil, Empty),
                Select("name", Nil, Empty),
                Select("profilePic", Nil, Empty)
              ))
            )
          ))
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

    val compiled = FragmentCompiler.compile(text)

    assert(compiled == Ior.Right(expected))

    val res = FragmentQueryInterpreter.run(compiled.right.get, FragmentSchema.queryType)
    //println(res)
    assert(res == expectedResult)
  }

  test("nested fragment query") {
    val text = """
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
      Select("user", Nil,
        Unique(FieldEquals("id", "1"),
          Group(List(
            Select("friends", Nil,
              Group(List(
                Select("id", Nil, Empty),
                Select("name", Nil, Empty),
                Select("profilePic", Nil, Empty)
              ))
            ),
            Select("mutualFriends", Nil,
              Group(List(
                Select("id", Nil, Empty),
                Select("name", Nil, Empty),
                Select("profilePic", Nil, Empty)
              ))
            )
          ))
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

    val compiled = FragmentCompiler.compile(text)

    assert(compiled == Ior.Right(expected))

    val res = FragmentQueryInterpreter.run(compiled.right.get, FragmentSchema.queryType)
    //println(res)
    assert(res == expectedResult)
  }

  test("typed fragment query") {
    val text = """
      query FragmentTyping {
        profiles {
          id
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

    val User = FragmentSchema.tpe("User")
    val Page = FragmentSchema.tpe("Page")

    val expected =
      Select("profiles", Nil,
        Group(List(
          Select("id", Nil, Empty),
          Narrow(User, Select("name", Nil, Empty)),
          Narrow(Page, Select("title", Nil, Empty))
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

    val compiled = FragmentCompiler.compile(text)

    assert(compiled == Ior.Right(expected))

    val res = FragmentQueryInterpreter.run(compiled.right.get, FragmentSchema.queryType)
    //println(res)
    assert(res == expectedResult)
  }

  test("inline fragment query") {
    val text = """
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

    val User = FragmentSchema.tpe("User")
    val Page = FragmentSchema.tpe("Page")

    val expected =
      Select("profiles", Nil,
        Group(List(
          Select("id", Nil, Empty),
          Narrow(User, Select("name", Nil, Empty)),
          Narrow(Page, Select("title", Nil, Empty))
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

    val compiled = FragmentCompiler.compile(text)

    assert(compiled == Ior.Right(expected))

    val res = FragmentQueryInterpreter.run(compiled.right.get, FragmentSchema.queryType)
    //println(res)
    assert(res == expectedResult)
  }
}

object FragmentSchema extends Schema {
  import ScalarType._

  val IdArg = InputValue("id", None, IDType, None)

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("user", None, List(IdArg), TypeRef("User"), false, None),
        Field("profiles", None, Nil, ListType(TypeRef("Profile")), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "User",
      description = None,
      fields = List(
        Field("id", None, Nil, StringType, false, None),
        Field("name", None, Nil, StringType, false, None),
        Field("profilePic", None, Nil, StringType, false, None),
        Field("friends", None, Nil, ListType(TypeRef("User")), false, None),
        Field("mutualFriends", None, Nil, ListType(TypeRef("User")), false, None),
      ),
      interfaces = List(TypeRef("Profile"))
    ),
    ObjectType(
      name = "Page",
      description = None,
      fields = List(
        Field("id", None, Nil, StringType, false, None),
        Field("title", None, Nil, StringType, false, None),
        Field("likers", None, Nil, ListType(TypeRef("User")), false, None),
      ),
      interfaces = List(TypeRef("Profile"))
    ),
    InterfaceType(
      name = "Profile",
      description = None,
      fields = List(
        Field("id", None, Nil, StringType, false, None),
      )
    )
  )

  val directives = Nil

  val User = tpe("User")
  val Page = tpe("Page")
}

object FragmentCompiler extends QueryCompiler(FragmentSchema) {
  val selectElaborator = new SelectElaborator(Map(
    FragmentSchema.tpe("Query").dealias -> {
      case Select("user", List(Binding("id", IDValue(id))), child) =>
        Select("user", Nil, Unique(FieldEquals("id", id), child)).rightIor
      case sel@Select("profiles", _, _) =>
        sel.rightIor
    }
  ))

  val phases = List(selectElaborator)
}

object FragmentData {
  sealed trait Profile {
    def id: String
  }
  case class User(id: String, name: String, profilePic: String, friendIds: List[String]) extends Profile {
    def friends: List[User] =
      friendIds.flatMap(fid => profiles.collect { case u: User if u.id == fid => u })
    def mutualFriends: List[User] =
      friendIds.flatMap(fid => profiles.collect { case u: User if u.id == fid && u.friendIds.contains(id) => u })
  }
  case class Page(id: String, title: String, likerIds: List[String]) extends Profile {
    def likers: List[User] =
      likerIds.flatMap(lid => profiles.collect { case u: User if u.id == lid => u })
  }

  val profiles = List(
    User("1", "Alice", "A", List("2", "3")),
    User("2", "Bob", "B", List("1")),
    User("3", "Carol", "C", List("1", "2")),
    Page("4", "GraphQL", List("1", "3")),
    Page("5", "Scala", List("2"))
  )
}

import FragmentData._

object FragmentQueryInterpreter extends DataTypeQueryInterpreter[Id](
  {
    case "user" =>
      (ListType(FragmentSchema.tpe("User")), profiles.collect { case u: User => u })
    case "profiles" =>
      (ListType(FragmentSchema.tpe("Profile")), profiles)
  },
  {
    case (p: Profile, "id")         => p.id
    case (u: User, "name")          => u.name
    case (u: User, "profilePic")    => u.profilePic
    case (u: User, "friends")       => u.friends
    case (u: User, "mutualFriends") => u.mutualFriends
    case (p: Page, "title")         => p.title
    case (p: Page, "likers")        => p.likers
  },
  narrows = {
    case (u: User, FragmentSchema.User) => u
    case (p: Page, FragmentSchema.Page) => p
  }
)
