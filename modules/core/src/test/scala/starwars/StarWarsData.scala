// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package starwars

import cats.Id
import cats.implicits._

import edu.gemini.grackle._
import Query._, Binding._
import QueryInterpreter.{ mkErrorResult, ProtoJson }

object StarWarsData {
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
}

object StarWarsQueryInterpreter extends QueryInterpreter[Id] {
  val schema = StarWarsSchema
  import schema._

  import StarWarsData._

  def runRootValue(query: Query): Result[ProtoJson] = {
    query match {
      case Select("hero", _, child) =>
        runValue(child, CharacterType, StarWarsCursor(R2D2))
      case Select("character" | "human", List(StringBinding("id", id)), child) =>
        runValue(child, NullableType(CharacterType), StarWarsCursor(characters.find(_.id == id)))
      case _ => mkErrorResult("Bad query")
    }
  }
}

case class StarWarsCursor(focus: Any) extends DataTypeCursor {
  import QueryInterpreter.mkErrorResult
  import StarWarsData._

  def mkCursor(focus: Any): Cursor = StarWarsCursor(focus)

  def hasField(fieldName: String): Boolean = (focus, fieldName) match {
    case (_: Character, "id" | "name" | "appearsIn" | "friends") => true
    case (_: Human, "homePlanet") => true
    case (_: Droid, "primaryFunction") => true
    case _ => false
  }

  def field(fieldName: String, args: Map[String, Any]): Result[Cursor] = (focus, fieldName) match {
    case (c: Character, "id") => mkCursor(c.id).rightIor
    case (c: Character, "name") => mkCursor(c.name).rightIor
    case (c: Character, "appearsIn") => mkCursor(c.appearsIn).rightIor
    case (c: Character, "friends") => mkCursor(c.friends).rightIor
    case (h: Human, "homePlanet") => mkCursor(h.homePlanet).rightIor
    case (d: Droid, "primaryFunction") => mkCursor(d.primaryFunction).rightIor
    case _ => mkErrorResult(s"No field '$fieldName'")
  }
}
