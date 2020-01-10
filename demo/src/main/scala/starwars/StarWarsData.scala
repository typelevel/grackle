// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package starwars

import cats.Id
import cats.implicits._

import edu.gemini.grackle._

import Query._, Binding._, Predicate._
import QueryCompiler._
import QueryInterpreter.mkErrorResult

import StarWarsData._

// The types and values for the in-memory Star Wars example.
// These are all ordinary Scala types with no Grackle dependencies.
//
object StarWarsData {
  // #model_types
  object Episode extends Enumeration {
    val NEWHOPE, EMPIRE, JEDI = Value
  }

  trait Character {
    def id: String
    def name: Option[String]
    def appearsIn: Option[List[Episode.Value]]
    def friends: Option[List[Character]]
  }

  case class Human private (
    id: String,
    name: Option[String],
    appearsIn: Option[List[Episode.Value]],
    homePlanet: Option[String]
  ) (friends0: => Option[List[Character]]) extends Character {
    lazy val friends = friends0
  }
  object Human {
    def apply(
      id: String,
      name: Option[String],
      appearsIn: Option[List[Episode.Value]],
      friends: => Option[List[Character]],
      homePlanet: Option[String]
    ) = new Human(id, name, appearsIn, homePlanet)(friends)
  }

  case class Droid private (
    id: String,
    name: Option[String],
    appearsIn: Option[List[Episode.Value]],
    primaryFunction: Option[String]
  ) (friends0: => Option[List[Character]]) extends Character {
    lazy val friends = friends0
  }
  object Droid {
    def apply(
      id: String,
      name: Option[String],
      appearsIn: Option[List[Episode.Value]],
      friends: => Option[List[Character]],
      primaryFunction: Option[String]
    ) = new Droid(id, name, appearsIn, primaryFunction)(friends)
  }
  // #model_types

  // #model_values
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

  // The character database ...
  val characters = List(
    LukeSkywalker, DarthVader, HanSolo, LeiaOrgana, WilhuffTarkin, C3PO, R2D2
  )
  // #model_values

  import Episode._

  // Mapping from Episode to its hero Character
  lazy val hero: Map[Value, Character] = Map(
    NEWHOPE -> R2D2,
    EMPIRE -> LukeSkywalker,
    JEDI -> R2D2
  )
}

object StarWarsQueryCompiler extends QueryCompiler(StarWarsSchema) {
  // #elaborator
  val elaborator = new SelectElaborator(Map(
    StarWarsSchema.tpe("Query").dealias -> {
      // The hero selector take an Episode argument and yields a single value. We use the
      // Unique operator to pick out the target using the FieldEquals predicate.
      case Select("hero", List(EnumBinding("episode", e)), child) =>
        Episode.values.find(_.toString == e.name).map { episode =>
          Select("hero", Nil, Unique(FieldEquals("id", hero(episode).id), child)).rightIor
        }.getOrElse(mkErrorResult(s"Unknown episode '${e.name}'"))

      // The character, human and droid selectors all take a single ID argument and yield a
      // single value (if any) or null. We use the Unique operator to pick out the target
      // using the FieldEquals predicate.
      case Select(f@("character" | "human" | "droid"), List(IDBinding("id", id)), child) =>
        Select(f, Nil, Unique(FieldEquals("id", id), child)).rightIor
    }
  ))
  // #elaborator
}

object StarWarsQueryInterpreter extends DataTypeQueryInterpreter[Id](
  // #root
  {
    // hero and character both start with [Character] and the full character database
    case "hero" | "character" =>
      (ListType(StarWarsSchema.tpe("Character")), characters)
    // human starts with [Human] and just the characters of type Human
    case "human" =>
      (ListType(StarWarsSchema.tpe("Human")), characters.collect { case h: Human => h })
    // droid starts with [Droid] and just the characters of type Droid
    case "droid" =>
      (ListType(StarWarsSchema.tpe("Droid")), characters.collect { case d: Droid => d })
  },
  // #root
  // #cursor
  {
    case (c: Character, "id")          => c.id
    case (c: Character, "name")        => c.name
    case (c: Character, "appearsIn")   => c.appearsIn
    case (c: Character, "friends")     => c.friends
    case (h: Human, "homePlanet")      => h.homePlanet
    case (d: Droid, "primaryFunction") => d.primaryFunction
  }
  // #cursor
)
