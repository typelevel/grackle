// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package starwars

import cats.Id
import cats.implicits._

import edu.gemini.grackle._

import Query._, Predicate._, Value._
import QueryCompiler._
import QueryInterpreter.{ mkErrorResult, mkOneError }
import generic._, semiauto._

import StarWarsData._

// The types and values for the in-memory Star Wars example.
// These are all ordinary Scala types with no Grackle dependencies.
//
object StarWarsData {
  // #schema
  val schema =
    Schema(
      """
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
    ).right.get
  // #schema

  val QueryType = schema.ref("Query")
  val CharacterType = schema.ref("Character")
  val HumanType = schema.ref("Human")
  val DroidType = schema.ref("Droid")

  // #model_types
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
      deriveInterfaceCursorBuilder[Character]
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
      deriveObjectCursorBuilder[Human].transformField("friends")(resolveFriends)
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
      deriveObjectCursorBuilder[Droid].transformField("friends")(resolveFriends)
  }

  def resolveFriends(c: Character): Result[Option[List[Character]]] =
    c.friends match {
      case None => None.rightIor
      case Some(ids) =>
        ids.traverse(id => characters.find(_.id == id).toRightIor(mkOneError(s"Bad id '$id'"))).map(_.some)
    }

  // #model_types

  // #model_values
  // The character database ...
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
  // #model_values

  import Episode._

  val Some(lukeSkywalker) = characters.find(_.id == "1000")
  val Some(r2d2) = characters.find(_.id == "2001")

  // Mapping from Episode to its hero Character
  val hero: Map[Value, Character] = Map(
    NEWHOPE -> r2d2,
    EMPIRE -> lukeSkywalker,
    JEDI -> r2d2
  )
}

object StarWarsQueryCompiler extends QueryCompiler(schema) {
  // #elaborator
  val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      // The hero selector take an Episode argument and yields a single value. We use the
      // Unique operator to pick out the target using the FieldEquals predicate.
      case Select("hero", List(Binding("episode", TypedEnumValue(e))), child) =>
        Episode.values.find(_.toString == e.name).map { episode =>
          Select("hero", Nil, Unique(Eql(FieldPath(List("id")), Const(hero(episode).id)), child)).rightIor
        }.getOrElse(mkErrorResult(s"Unknown episode '${e.name}'"))

      // The character, human and droid selectors all take a single ID argument and yield a
      // single value (if any) or null. We use the Unique operator to pick out the target
      // using the FieldEquals predicate.
      case Select(f@("character" | "human" | "droid"), List(Binding("id", IDValue(id))), child) =>
        Select(f, Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)).rightIor
    }
  ))
  // #elaborator

  val phases = List(selectElaborator)
}

object StarWarsQueryInterpreter extends GenericQueryInterpreter[Id](
  // #root
  {
    // hero and character both start with [Character] and the full character database
    case "hero" | "character" =>
      CursorBuilder[List[Character]].build(characters, ListType(CharacterType))
    // human starts with [Human] and just the characters of type Human
    case "human" =>
      CursorBuilder[List[Human]].build(characters.collect { case h: Human => h }, ListType(HumanType))
    // droid starts with [Droid] and just the characters of type Droid
    case "droid" =>
      CursorBuilder[List[Droid]].build(characters.collect { case d: Droid => d }, ListType(DroidType))
  },
  // #root
)
