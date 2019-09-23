// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini
package grackle
package starwars

import cats.Id
import io.circe.{ Json, JsonObject}

import Query._, Binding._
import Schema._

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
    lazy val friends = friends0
  }
  object Human {
    def apply(id: String, name: Option[String], appearsIn: List[Episode.Value], friends: => List[Character], homePlanet: Option[String]) =
      new Human(id, name, appearsIn, homePlanet)(friends)
  }

  case class Droid private (id: String, name: Option[String], appearsIn: List[Episode.Value], primaryFunction: Option[String])
    (friends0: => List[Character]) extends Character {
    lazy val friends = friends0
  }
  object Droid {
    def apply(id: String, name: Option[String], appearsIn: List[Episode.Value], friends: => List[Character], primaryFunction: Option[String]) =
      new Droid(id, name, appearsIn, primaryFunction)(friends)
  }

  case class Root(characters: List[Character]) {
    def schema: Schema = StarWarsSchema.schema
  }

  val root = Root(List(
    LukeSkywalker, DarthVader, HanSolo, LeiaOrgana, WilhuffTarkin, C3PO, R2D2
  ))

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

object StarWarsQueryInterpreter extends QueryInterpreter[Id, Json] {
  import StarWarsData._

  def run(q: Query): Json = Json.obj("data" -> Json.fromJsonObject(run(q, root, root.schema.queryType, root, JsonObject.empty)))

  def run[T](q: Query, root: Root, schema: Type, elem: T, acc: JsonObject): JsonObject = {
    println(s"schema: $schema")

    def checkField(fieldName: String): Unit =
      assert(schemaOfField(root.schema, schema, fieldName).isDefined)

    def field(fieldName: String): Type =
      schemaOfField(root.schema, schema, fieldName).get

    (q, elem) match {
      case (Nest(Select("character", List(StringBinding("id", id))), q), Root(characters)) =>
        checkField("character")
        val child = characters.find(_.id == id).map(character => run(q, root, field("character"), character, JsonObject.empty))
        acc.add("character", child.map(child => Json.fromJsonObject(child)).getOrElse(Json.Null))

      case (Select("name", Nil), character: Character) =>
        checkField("name")
        acc.add("name", character.name.map(Json.fromString).getOrElse(Json.Null))

      case (Nest(Select("friends", Nil), q), character: Character) =>
        checkField("friends")
        val children = character.friends.map(friend => run(q, root, field("friends"), friend, JsonObject.empty))
        acc.add("friends", Json.fromValues(children.map(Json.fromJsonObject)))

      case (Group(siblings), elem) =>
        siblings.foldLeft(acc)((acc, q) => run(q, root, schema, elem, acc))
    }
  }

  def schemaOfField(schema: Schema, tpe: Type, fieldName: String): Option[Type] = tpe match {
    case NonNullType(tpe) => schemaOfField(schema, tpe, fieldName)
    case ListType(tpe) => schemaOfField(schema, tpe, fieldName)
    case TypeRef(tpnme) => schema.types.find(_.name == tpnme).flatMap(tpe => schemaOfField(schema, tpe, fieldName))
    case ObjectType(_, _, fields, _) => fields.find(_.name == fieldName).map(_.tpe)
    case InterfaceType(_, _, fields) => fields.find(_.name == fieldName).map(_.tpe)
    case _ => None
  }
}
