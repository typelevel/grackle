// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini
package grackle
package starwars

object StarWarsData {
  object Episode extends Enumeration {
    val NEWHOPE, EMPIRE, JEDI = Value
  }

  trait Character {
    def id: String
    def name: Option[String]
    def appearsIn: List[Episode.Value]
    def friends: List[Character]
  }

  case class Human private (id: String, name: Option[String], appearsIn: List[Episode.Value], homePlanet: Option[String])
    (friends0: => List[Character]) extends Character {
    def friends = friends0
  }
  object Human {
    def apply(id: String, name: Option[String], appearsIn: List[Episode.Value], friends: => List[Character], homePlanet: Option[String]) =
      new Human(id, name, appearsIn, homePlanet)(friends)
  }

  case class Droid private (id: String, name: Option[String], appearsIn: List[Episode.Value], primaryFunction: Option[String])
    (friends0: => List[Character]) extends Character {
    def friends = friends0
  }
  object Droid {
    def apply(id: String, name: Option[String], appearsIn: List[Episode.Value], friends: => List[Character], primaryFunction: Option[String]) =
      new Droid(id, name, appearsIn, primaryFunction)(friends)
  }

  val characters = List[Character](
    LukeSkywalker, DarthVader, HanSolo, LeiaOrgana, WilhuffTarkin, C3PO, R2D2
  )

  lazy val LukeSkywalker: Character =
    Human(
      id = "1000",
      name = Some("Luke Skywalker"),
      friends = List(HanSolo, LeiaOrgana, C3PO, R2D2),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Tatooine"))
  lazy val DarthVader: Character =
    Human(
      id = "1001",
      name = Some("Darth Vader"),
      friends = List(WilhuffTarkin),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Tatooine"))
  lazy val HanSolo: Character =
    Human(
      id = "1002",
      name = Some("Han Solo"),
      friends = List(LukeSkywalker, LeiaOrgana, R2D2),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = None)
  lazy val LeiaOrgana: Character =
    Human(
      id = "1003",
      name = Some("Leia Organa"),
      friends = List(LukeSkywalker, HanSolo, C3PO, R2D2),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Alderaan"))
  lazy val WilhuffTarkin: Character =
    Human(
      id = "1004",
      name = Some("Wilhuff Tarkin"),
      friends = List(DarthVader),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = None)
  lazy val C3PO: Character =
    Droid(
      id = "2000",
      name = Some("C-3PO"),
      friends = List(LukeSkywalker, HanSolo, LeiaOrgana, R2D2),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      primaryFunction = Some("Protocol"))
  lazy val R2D2: Character =
    Droid(
      id = "2001",
      name = Some("R2-D2"),
      friends = List(LukeSkywalker, HanSolo, LeiaOrgana),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      primaryFunction = Some("Astromech"))
}
