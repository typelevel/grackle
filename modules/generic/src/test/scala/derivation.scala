// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import java.time._

import cats._
import cats.data.Ior
import cats.implicits._
import cats.tests.CatsSuite
import io.circe.Json

import edu.gemini.grackle.syntax._
import Cursor.Context
import Query._, Path._, Predicate._, Value._
import QueryCompiler._
import QueryInterpreter.{ mkErrorResult, mkOneError }
import semiauto._

object StarWarsData {
  import StarWarsMapping.{CharacterType, DroidType, HumanType}

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
      case None => None.rightIor
      case Some(ids) =>
        ids.traverse(id => characters.find(_.id == id).toRightIor(mkOneError(s"Bad id '$id'"))).map(_.some)
    }

  case class Human(
    id: String,
    name: Option[String],
    appearsIn: Option[List[Episode.Value]],
    friends: Option[List[String]],
    homePlanet: Option[String]
  ) extends Character

  object Human {
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
      homePlanet = Some("Tatooine")
    ),
    Human(
      id = "1001",
      name = Some("Darth Vader"),
      friends = Some(List("1004")),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      homePlanet = Some("Tatooine")
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
      homePlanet = Some("Alderaan")
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

  val Some(lukeSkywalker: Human) = characters.find(_.id == "1000")
  val Some(r2d2: Droid) = characters.find(_.id == "2001")

  // Mapping from Episode to its hero Character
  lazy val hero: Map[Value, Character] = Map(
    NEWHOPE -> r2d2,
    EMPIRE -> lukeSkywalker,
    JEDI -> r2d2
  )
}

object StarWarsMapping extends GenericMapping[Id] {
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
        friends: [Character!]
        appearsIn: [Episode!]
      }
      type Human implements Character {
        id: String!
        name: String
        friends: [Character!]
        appearsIn: [Episode!]
        homePlanet: String
      }
      type Droid implements Character {
        id: String!
        name: String
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
            GenericRoot("hero", characters),
            GenericRoot("character", characters),
            GenericRoot("human", characters.collect { case h: Human => h }),
            GenericRoot("droid", characters.collect { case d: Droid => d })
          )
      )
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("hero", List(Binding("episode", TypedEnumValue(e))), child) =>
        Episode.values.find(_.toString == e.name).map { episode =>
          Select("hero", Nil, Unique(Filter(Eql(UniquePath(List("id")), Const(hero(episode).id)), child))).rightIor
        }.getOrElse(mkErrorResult(s"Unknown episode '${e.name}'"))

      case Select(f@("character" | "human" | "droid"), List(Binding("id", IDValue(id))), child) =>
        Select(f, Nil, Unique(Filter(Eql(UniquePath(List("id")), Const(id)), child))).rightIor
    }
  ))
}

import StarWarsData._, StarWarsMapping._

final class DerivationSpec extends CatsSuite {
  test("primitive types have leaf cursor builders") {
    val i =
      for {
        c <- CursorBuilder[Int].build(Context.empty, 23)
        _  = assert(c.isLeaf)
        l  <- c.asLeaf
      } yield l
    assert(i == Ior.Right(Json.fromInt(23)))

    val s =
      for {
        c <- CursorBuilder[String].build(Context.empty, "foo")
        _  = assert(c.isLeaf)
        l  <- c.asLeaf
      } yield l
    assert(s == Ior.Right(Json.fromString("foo")))

    val b =
      for {
        c <- CursorBuilder[Boolean].build(Context.empty, true)
        _  = assert(c.isLeaf)
        l  <- c.asLeaf
      } yield l
    assert(b == Ior.Right(Json.fromBoolean(true)))

    val f =
      for {
        c <- CursorBuilder[Double].build(Context.empty, 13.0)
        _  = assert(c.isLeaf)
        l  <- c.asLeaf
      } yield l
    assert(f == Ior.Right(Json.fromDouble(13.0).get))
  }

  test("types with a Circe Encoder instance have leaf cursor builders") {
    val z =
      for {
        c <- CursorBuilder.leafCursorBuilder[ZonedDateTime].build(Context.empty, ZonedDateTime.parse("2020-03-25T16:24:06.081Z"))
        _  = assert(c.isLeaf)
        l  <- c.asLeaf
      } yield l
    assert(z == Ior.Right(Json.fromString("2020-03-25T16:24:06.081Z")))
  }

  test("Scala Enumeration types have leaf cursor builders") {
    val e =
      for {
        c <- CursorBuilder.enumerationCursorBuilder[Episode.Value].build(Context.empty, Episode.JEDI)
        _  = assert(c.isLeaf)
        l  <- c.asLeaf
      } yield l
    assert(e == Ior.Right(Json.fromString("JEDI")))
  }

  test("product types have cursor builders") {
    val name =
      for {
        c <- CursorBuilder[Human].build(Context.empty, lukeSkywalker)
        f <- c.field("name", None)
        n <- f.asNullable.flatMap(_.toRightIor(mkOneError("missing")))
        l <- n.asLeaf
      } yield l
    assert(name == Ior.Right(Json.fromString("Luke Skywalker")))
  }

  test("cursor builders can be resolved for nested types") {
    val appearsIn =
      for {
        c <- CursorBuilder[Character].build(Context.empty, lukeSkywalker)
        f <- c.field("appearsIn", None)
        n <- f.asNullable.flatMap(_.toRightIor(mkOneError("missing")))
        l <- n.asList
        s <- l.traverse(_.asLeaf)
      } yield s
    assert(appearsIn == Ior.Right(List(Json.fromString("NEWHOPE"), Json.fromString("EMPIRE"), Json.fromString("JEDI"))))
  }

  test("default cursor builders can be customised by mapping fields") {
    val friends =
      for {
        c <- CursorBuilder[Human].build(Context.empty, lukeSkywalker)
        f <- c.field("friends", None)
        n <- f.asNullable.flatMap(_.toRightIor(mkOneError("missing")))
        l <- n.asList
        m <- l.traverse(_.field("name", None))
        p <- m.traverse(_.asNullable.flatMap(_.toRightIor(mkOneError("missing"))))
        q <- p.traverse(_.asLeaf)
      } yield q
    assert(friends == Ior.Right(List(Json.fromString("Han Solo"), Json.fromString("Leia Organa"), Json.fromString("C-3PO"), Json.fromString("R2-D2"))))
  }

  test("sealed ADTs have narrowable cursor builders") {
    val homePlanets =
      for {
        c <- CursorBuilder[Character].build(Context.empty, lukeSkywalker)
        h <- c.narrow(HumanType)
        m <- h.field("homePlanet", None)
        n <- m.asNullable.flatMap(_.toRightIor(mkOneError("missing")))
        l <- n.asLeaf
      } yield l
    assert(homePlanets == Ior.Right(Json.fromString("Tatooine")))
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
    //println(res)

    assert(res == expected)
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
    //println(res)

    assert(res == expected)
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
    //println(res)

    assert(res == expected)
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
    //println(res)

    assert(res == expected)
  }
}
