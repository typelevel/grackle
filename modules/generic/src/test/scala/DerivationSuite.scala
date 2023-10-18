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

import java.time._

import cats.effect.IO
import cats.implicits._
import io.circe.Json
import io.circe.literal._
import munit.CatsEffectSuite

import grackle.syntax._
import Query._, Predicate._, Value._
import QueryCompiler._
import ScalarType._

object StarWarsData {
  import StarWarsMapping._
  import semiauto._

  object Episode extends Enumeration {
    val NEWHOPE, EMPIRE, JEDI = Value
  }

  sealed trait Character {
    def id: String
    def name: Option[String]
    def appearsIn: Option[List[Episode.Value]]
    def friends: Option[List[String]]
  }

  object Character {
    implicit val cursorBuilder: CursorBuilder[Character] =
      deriveInterfaceCursorBuilder[Character](CharacterType)
  }

  def resolveFriends(c: Character): Result[Option[List[Character]]] =
    c.friends match {
      case None => None.success
      case Some(ids) =>
        ids.traverse(id => characters.find(_.id == id).toResultOrError(s"Bad id '$id'")).map(_.some)
    }

  case class Planet(value: String)

  case class Human(
    id: String,
    name: Option[String],
    appearsIn: Option[List[Episode.Value]],
    friends: Option[List[String]],
    homePlanet: Option[Planet]
  ) extends Character

  object Human {
    implicit val planetCursorBuilder: CursorBuilder[Planet] =
      CursorBuilder[String].contramap(_.value)
    implicit val cursorBuilder: CursorBuilder[Human] =
      deriveObjectCursorBuilder[Human](HumanType)
        .transformField("friends")(resolveFriends)
  }

  case class Droid(
    id: String,
    name: Option[String],
    appearsIn: Option[List[Episode.Value]],
    friends: Option[List[String]],
    primaryFunction: Option[String]
  ) extends Character

  object Droid {
    implicit val cursorBuilder: CursorBuilder[Droid] =
      deriveObjectCursorBuilder[Droid](DroidType)
        .transformField("friends")(resolveFriends)
  }

  val characters: List[Character] = List(
    Human(
      id = "1000",
      name = Some("Luke Skywalker"),
      friends = Some(List("1002", "1003", "2000", "2001")),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      homePlanet = Some(Planet("Tatooine"))
    ),
    Human(
      id = "1001",
      name = Some("Darth Vader"),
      friends = Some(List("1004")),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      homePlanet = Some(Planet("Tatooine"))
    ),
    Human(
      id = "1002",
      name = Some("Han Solo"),
      friends = Some(List("1000", "1003", "2001")),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      homePlanet = None
    ),
    Human(
      id = "1003",
      name = Some("Leia Organa"),
      friends = Some(List("1000", "1002", "2000", "2001")),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      homePlanet = Some(Planet("Alderaan"))
    ),
    Human(
      id = "1004",
      name = Some("Wilhuff Tarkin"),
      friends = Some(List("1001")),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      homePlanet = None
    ),
    Droid(
      id = "2000",
      name = Some("C-3PO"),
      friends = Some(List("1000", "1002", "1003", "2001")),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      primaryFunction = Some("Protocol")
    ),
    Droid(
      id = "2001",
      name = Some("R2-D2"),
      friends = Some(List("1000", "1002", "1003")),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      primaryFunction = Some("Astromech")
    )
  )

  import Episode._

  val Some(lukeSkywalker: Human) = characters.find(_.id == "1000") : @unchecked
  val Some(r2d2: Droid) = characters.find(_.id == "2001") : @unchecked

  // Mapping from Episode to its hero Character
  lazy val hero: Map[Value, Character] = Map(
    NEWHOPE -> r2d2,
    EMPIRE -> lukeSkywalker,
    JEDI -> r2d2
  )
}

object StarWarsMapping extends GenericMapping[IO] {
  import StarWarsData.{characters, hero, Droid, Human, Episode}

  val schema =
    schema"""
      type Query {
        hero(episode: Episode!): Character!
        character(id: ID!): Character
        human(id: ID!): Human
        droid(id: ID!): Droid
      }
      enum Episode {
        NEWHOPE
        EMPIRE
        JEDI
      }
      interface Character {
        id: String!
        name: String
        numberOfFriends: Int
        friends: [Character!]
        appearsIn: [Episode!]
      }
      type Human implements Character {
        id: String!
        name: String
        numberOfFriends: Int
        friends: [Character!]
        appearsIn: [Episode!]
        homePlanet: String
      }
      type Droid implements Character {
        id: String!
        name: String
        numberOfFriends: Int
        friends: [Character!]
        appearsIn: [Episode!]
        primaryFunction: String
      }
    """

  val QueryType = schema.ref("Query")
  val EpisodeType = schema.ref("Episode")
  val CharacterType = schema.ref("Character")
  val HumanType = schema.ref("Human")
  val DroidType = schema.ref("Droid")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            GenericField("hero", characters),
            GenericField("character", characters),
            GenericField("human", characters.collect { case h: Human => h }),
            GenericField("droid", characters.collect { case d: Droid => d })
          )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "hero", List(Binding("episode", EnumValue(e)))) =>
      for {
        episode <- Elab.liftR(Episode.values.find(_.toString == e).toResult(s"Unknown episode '$e'"))
        _       <- Elab.transformChild(child => Unique(Filter(Eql(CharacterType / "id", Const(hero(episode).id)), child)))
      } yield ()

    case (QueryType, "character" | "human" | "droid", List(Binding("id", IDValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CharacterType / "id", Const(id)), child)))

    case (CharacterType | HumanType | DroidType, "numberOfFriends", Nil) =>
      Elab.transformChild(_ => Count(Select("friends")))
  }
}

import StarWarsData._, StarWarsMapping._

final class DerivationSuite extends CatsEffectSuite {
  test("primitive types have leaf cursor builders") {
    val i =
      for {
        c <- CursorBuilder[Int].build(Context(IntType), 23)
        _  = assert(c.isLeaf)
        l  <- c.asLeaf
      } yield l
    assertEquals(i, Result.Success(Json.fromInt(23)))

    val s =
      for {
        c <- CursorBuilder[String].build(Context(StringType), "foo")
        _  = assert(c.isLeaf)
        l  <- c.asLeaf
      } yield l
    assertEquals(s, Result.Success(Json.fromString("foo")))

    val b =
      for {
        c <- CursorBuilder[Boolean].build(Context(BooleanType), true)
        _  = assert(c.isLeaf)
        l  <- c.asLeaf
      } yield l
    assertEquals(b, Result.Success(Json.fromBoolean(true)))

    val f =
      for {
        c <- CursorBuilder[Double].build(Context(FloatType), 13.0)
        _  = assert(c.isLeaf)
        l  <- c.asLeaf
      } yield l
    assertEquals(f, Result.Success(Json.fromDouble(13.0).get))
  }

  test("types with a Circe Encoder instance have leaf cursor builders") {
    val z =
      for {
        c <- CursorBuilder.leafCursorBuilder[ZonedDateTime].build(Context(AttributeType), ZonedDateTime.parse("2020-03-25T16:24:06.081Z"))
        _  = assert(c.isLeaf)
        l  <- c.asLeaf
      } yield l
    assertEquals(z, Result.Success(Json.fromString("2020-03-25T16:24:06.081Z")))
  }

  test("Scala Enumeration types have leaf cursor builders") {
    val e =
      for {
        c <- CursorBuilder.enumerationCursorBuilder[Episode.Value].build(Context(EpisodeType), Episode.JEDI)
        _  = assert(c.isLeaf)
        l  <- c.asLeaf
      } yield l
    assertEquals(e, Result.Success(Json.fromString("JEDI")))
  }

  test("product types have cursor builders") {
    val name =
      for {
        c <- CursorBuilder[Human].build(Context(HumanType), lukeSkywalker)
        f <- c.field("name", None)
        n <- f.asNullable.flatMap(_.toResultOrError("missing"))
        l <- n.asLeaf
      } yield l
    assertEquals(name, Result.Success(Json.fromString("Luke Skywalker")))
  }

  test("cursor builders can be resolved for nested types") {
    val appearsIn =
      for {
        c <- CursorBuilder[Character].build(Context(CharacterType), lukeSkywalker)
        f <- c.field("appearsIn", None)
        n <- f.asNullable.flatMap(_.toResultOrError("missing"))
        l <- n.asList(List)
        s <- l.traverse(_.asLeaf)
      } yield s
    assertEquals(appearsIn, Result.Success(List(Json.fromString("NEWHOPE"), Json.fromString("EMPIRE"), Json.fromString("JEDI"))))
  }

  test("default cursor builders can be customised by mapping fields") {
    val friends =
      for {
        c <- CursorBuilder[Human].build(Context(HumanType), lukeSkywalker)
        f <- c.field("friends", None)
        n <- f.asNullable.flatMap(_.toResultOrError("missing"))
        l <- n.asList(List)
        m <- l.traverse(_.field("name", None))
        p <- m.traverse(_.asNullable.flatMap(_.toResultOrError("missing")))
        q <- p.traverse(_.asLeaf)
      } yield q
    assertEquals(friends, Result.Success(List(Json.fromString("Han Solo"), Json.fromString("Leia Organa"), Json.fromString("C-3PO"), Json.fromString("R2-D2"))))
  }

  test("sealed ADTs have narrowable cursor builders") {
    val homePlanets =
      for {
        c <- CursorBuilder[Character].build(Context(CharacterType), lukeSkywalker)
        h <- c.narrow(HumanType)
        m <- h.field("homePlanet", None)
        n <- m.asNullable.flatMap(_.toResultOrError("missing"))
        l <- n.asLeaf
      } yield l
    assertEquals(homePlanets, Result.Success(Json.fromString("Tatooine")))
  }

  test("simple query") {
    val query = """
      query {
        character(id: "1000") {
          name
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "character" : {
            "name" : "Luke Skywalker"
          }
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("subtype query") {
    val query = """
      query {
        human(id: "1003") {
          name
          homePlanet
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "human" : {
            "name" : "Leia Organa",
            "homePlanet" : "Alderaan"
          }
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("simple nested query (1)") {
    val query = """
      query {
        character(id: "1000") {
          name
          friends {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "character" : {
            "name" : "Luke Skywalker",
            "friends" : [
              {
                "name" : "Han Solo"
              },
              {
                "name" : "Leia Organa"
              },
              {
                "name" : "C-3PO"
              },
              {
                "name" : "R2-D2"
              }
            ]
          }
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("fragment query") {
    val query = """
      query {
        character(id: "1000") {
          name
          ... on Human {
            homePlanet
          }
          friends {
            name
            ... on Human {
              homePlanet
            }
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "character" : {
            "name" : "Luke Skywalker",
            "homePlanet" : "Tatooine",
            "friends" : [
              {
                "name" : "Han Solo",
                "homePlanet" : null
              },
              {
                "name" : "Leia Organa",
                "homePlanet" : "Alderaan"
              },
              {
                "name" : "C-3PO"
              },
              {
                "name" : "R2-D2"
              }
            ]
          }
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("supertype fragment query") {
    val query = """
      query {
        human(id: "1000") {
          id
          ... on Character {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "human" : {
            "id" : "1000",
            "name" : "Luke Skywalker"
          }
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("count") {
    val query = """
      query {
        character(id: "1000") {
          name
          numberOfFriends
          friends {
            name
          }
        }
        human(id: "1001") {
          name
          numberOfFriends
          friends {
            name
          }
        }
        droid(id: "2001") {
          name
          numberOfFriends
          friends {
            name
          }
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "character" : {
            "name" : "Luke Skywalker",
            "numberOfFriends" : 4,
            "friends" : [
              {
                "name" : "Han Solo"
              },
              {
                "name" : "Leia Organa"
              },
              {
                "name" : "C-3PO"
              },
              {
                "name" : "R2-D2"
              }
            ]
          },
          "human" : {
            "name" : "Darth Vader",
            "numberOfFriends" : 1,
            "friends" : [
              {
                "name" : "Wilhuff Tarkin"
              }
            ]
          },
          "droid" : {
            "name" : "R2-D2",
            "numberOfFriends" : 3,
            "friends" : [
              {
                "name" : "Luke Skywalker"
              },
              {
                "name" : "Han Solo"
              },
              {
                "name" : "Leia Organa"
              }
            ]
          }
        }
      }
    """

    val res = StarWarsMapping.compileAndRun(query)

    assertIO(res, expected)
  }
}
