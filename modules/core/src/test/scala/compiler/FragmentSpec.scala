// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package compiler

import cats.Id
import cats.data.Ior
import cats.implicits._
import cats.tests.CatsSuite

import edu.gemini.grackle._
import edu.gemini.grackle.syntax._
import Query._, Path._, Predicate._, Value._
import QueryCompiler._

final class FragmentSuite extends CatsSuite {
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
      Select("user", Nil,
        Unique(
          Filter(Eql(UniquePath(List("id")), Const("1")),
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

    assert(compiled.map(_.query) == Ior.Right(expected))

    val res = FragmentMapping.run(compiled.right.get).compile.toList.head
    //println(res)
    assert(res == expectedResult)
  }

  test("nested fragment query") {
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
      Select("user", Nil,
        Unique(
          Filter(Eql(UniquePath(List("id")), Const("1")),
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

    assert(compiled.map(_.query) == Ior.Right(expected))

    val res = FragmentMapping.run(compiled.right.get).compile.toList.head
    //println(res)
    assert(res == expectedResult)
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
        Nil,
        Group(List(
          Select("id", Nil, Empty),
          Introspect(FragmentMapping.schema, Select("__typename", Nil, Empty)),
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

    assert(compiled.map(_.query) == Ior.Right(expected))

    val res = FragmentMapping.run(compiled.right.get).compile.toList.head
    //println(res)
    assert(res == expectedResult)
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

    val compiled = FragmentMapping.compiler.compile(query)

    assert(compiled.map(_.query) == Ior.Right(expected))

    val res = FragmentMapping.run(compiled.right.get).compile.toList.head
    //println(res)
    assert(res == expectedResult)
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
        Select("user", Nil,
          Unique(
            Filter(Eql(UniquePath(List("id")), Const("1")),
              Select("favourite", Nil,
                Group(List(
                  Introspect(FragmentMapping.schema, Select("__typename", Nil, Empty)),
                  Group(List(
                    Narrow(User, Select("id", Nil, Empty)),
                    Narrow(User, Select("name", Nil, Empty)))),
                  Group(List(
                    Narrow(Page, Select("id", Nil, Empty)),
                    Narrow(Page, Select("title", Nil, Empty))
                  ))
                ))
              )
            )
          )
        ),
        Rename("page", Select("user", Nil,
          Unique(
            Filter(Eql(UniquePath(List("id")), Const("2")),
              Select("favourite", Nil,
                Group(List(
                  Introspect(FragmentMapping.schema, Select("__typename", Nil, Empty)),
                  Group(List(
                    Narrow(User, Select("id", Nil, Empty)),
                    Narrow(User, Select("name", Nil, Empty))
                  )),
                  Group(List(
                    Narrow(Page, Select("id", Nil, Empty)),
                    Narrow(Page, Select("title", Nil, Empty))
                  ))
                ))
              )
            )
          )
        ))
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

    assert(compiled.map(_.query) == Ior.Right(expected))

    val res = FragmentMapping.run(compiled.right.get).compile.toList.head
    //println(res)
    assert(res == expectedResult)
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

object FragmentMapping extends ValueMapping[Id] {
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
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            ValueRoot("user", profiles.collect { case u: User => u }),
            ValueRoot("profiles", profiles)
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

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("user", List(Binding("id", IDValue(id))), child) =>
        Select("user", Nil, Unique(Filter(Eql(UniquePath(List("id")), Const(id)), child))).rightIor
      case sel@Select("profiles", _, _) =>
        sel.rightIor
    }
  ))
}
