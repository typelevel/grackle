// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package starwars

import cats.Id
import cats.implicits._

import edu.gemini.grackle._

import Query._, Predicate._, Value._
import QueryCompiler._

import StarWarsData._

object StarWarsData {
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

  object Episode extends Enumeration {
    val NEWHOPE, EMPIRE, JEDI = Value
  }

  trait Character {
    def id: String
    def name: Option[String]
    def appearsIn: Option[List[Episode.Value]]
    def friends: Option[List[Character]]
  }

  case class Human private (id: String, name: Option[String], appearsIn: Option[List[Episode.Value]], homePlanet: Option[String])
    (friends0: => Option[List[Character]]) extends Character {
    lazy val friends = friends0
  }
  object Human {
    def apply(id: String, name: Option[String], appearsIn: Option[List[Episode.Value]], friends: => Option[List[Character]], homePlanet: Option[String]) =
      new Human(id, name, appearsIn, homePlanet)(friends)
  }

  case class Droid private (id: String, name: Option[String], appearsIn: Option[List[Episode.Value]], primaryFunction: Option[String])
    (friends0: => Option[List[Character]]) extends Character {
    lazy val friends = friends0
  }
  object Droid {
    def apply(id: String, name: Option[String], appearsIn: Option[List[Episode.Value]], friends: => Option[List[Character]], primaryFunction: Option[String]) =
      new Droid(id, name, appearsIn, primaryFunction)(friends)
  }

  val characters = List(
    LukeSkywalker, DarthVader, HanSolo, LeiaOrgana, WilhuffTarkin, C3PO, R2D2
  )

  lazy val LukeSkywalker: Character =
    Human(
      id = "1000",
      name = Some("Luke Skywalker"),
      friends = Some(List(HanSolo, LeiaOrgana, C3PO, R2D2)),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      homePlanet = Some("Tatooine"))
  lazy val DarthVader: Character =
    Human(
      id = "1001",
      name = Some("Darth Vader"),
      friends = Some(List(WilhuffTarkin)),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      homePlanet = Some("Tatooine"))
  lazy val HanSolo: Character =
    Human(
      id = "1002",
      name = Some("Han Solo"),
      friends = Some(List(LukeSkywalker, LeiaOrgana, R2D2)),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      homePlanet = None)
  lazy val LeiaOrgana: Character =
    Human(
      id = "1003",
      name = Some("Leia Organa"),
      friends = Some(List(LukeSkywalker, HanSolo, C3PO, R2D2)),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      homePlanet = Some("Alderaan"))
  lazy val WilhuffTarkin: Character =
    Human(
      id = "1004",
      name = Some("Wilhuff Tarkin"),
      friends = Some(List(DarthVader)),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      homePlanet = None)
  lazy val C3PO: Character =
    Droid(
      id = "2000",
      name = Some("C-3PO"),
      friends = Some(List(LukeSkywalker, HanSolo, LeiaOrgana, R2D2)),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      primaryFunction = Some("Protocol"))
  lazy val R2D2: Character =
    Droid(
      id = "2001",
      name = Some("R2-D2"),
      friends = Some(List(LukeSkywalker, HanSolo, LeiaOrgana)),
      appearsIn = Some(List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI)),
      primaryFunction = Some("Astromech"))

  import Episode._

  lazy val hero: Map[Value, Character] = Map(
    NEWHOPE -> R2D2,
    EMPIRE -> LukeSkywalker,
    JEDI -> R2D2
  )
}

object StarWarsQueryCompiler extends QueryCompiler(StarWarsData.schema) {
  val selectElaborator = new SelectElaborator(Map(
    StarWarsData.schema.ref("Query") -> {
      case Select("hero", List(Binding("episode", TypedEnumValue(e))), child) =>
        val episode = Episode.values.find(_.toString == e.name).get
        Select("hero", Nil, Unique(Eql(FieldPath(List("id")), Const(hero(episode).id)), child)).rightIor
      case Select(f@("character" | "human" | "droid"), List(Binding("id", IDValue(id))), child) =>
        Select(f, Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)).rightIor
    }
  ))

  val phases = List(selectElaborator)
}

object StarWarsQueryInterpreter extends DataTypeQueryInterpreter[Id](
  {
    case "hero" | "character" => (ListType(StarWarsData.schema.ref("Character")), characters)
    case "human" => (ListType(StarWarsData.schema.ref("Human")), characters.collect { case h: Human => h })
    case "droid" => (ListType(StarWarsData.schema.ref("Droid")), characters.collect { case d: Droid => d })
  },
  {
    case (c: Character, "id")          => c.id
    case (c: Character, "name")        => c.name
    case (c: Character, "appearsIn")   => c.appearsIn
    case (c: Character, "friends")     => c.friends
    case (h: Human, "homePlanet")      => h.homePlanet
    case (d: Droid, "primaryFunction") => d.primaryFunction
  }
)
