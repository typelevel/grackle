// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package starwars

import cats.Id
import cats.implicits._

import edu.gemini.grackle._

import Query._, Predicate._, Value._
import QueryCompiler._

object StarWarsData {
  object Episode extends Enumeration {
    val NEWHOPE, EMPIRE, JEDI = Value
  }

  trait Character {
    def id: String
    def name: Option[String]
    def appearsIn: Option[List[Episode.Value]]
    def friends: Option[List[String]]
  }

  case class Human(
    id: String,
    name: Option[String],
    appearsIn: Option[List[Episode.Value]],
    friends: Option[List[String]],
    homePlanet: Option[String]
  ) extends Character

  case class Droid(
    id: String,
    name: Option[String],
    appearsIn: Option[List[Episode.Value]],
    friends: Option[List[String]],
    primaryFunction: Option[String]
  ) extends Character

  def resolveFriends(c: Character): Option[List[Character]] =
    c.friends match {
      case None => None
      case Some(ids) =>
        ids.traverse(id => characters.find(_.id == id))
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

  val Some(lukeSkywalker) = characters.find(_.id == "1000")
  val Some(r2d2) = characters.find(_.id == "2001")

  val hero: Map[Value, Character] = Map(
    NEWHOPE -> r2d2,
    EMPIRE -> lukeSkywalker,
    JEDI -> r2d2
  )
}

object StarWarsMapping extends ValueMapping[Id] {
  import StarWarsData.{characters, hero, resolveFriends, Character, Droid, Episode, Human}

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

  val QueryType = schema.ref("Query")
  val CharacterType = schema.ref("Character")
  val HumanType = schema.ref("Human")
  val DroidType = schema.ref("Droid")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            ValueRoot("hero", characters),
            ValueRoot("character", characters),
            ValueRoot("human", characters.collect { case h: Human => h }),
            ValueRoot("droid", characters.collect { case d: Droid => d })
          )
      ),
      ValueObjectMapping[Character](
        tpe = CharacterType,
        fieldMappings =
          List(
            ValueField("id", _.id),
            ValueField("name", _.name),
            ValueField("appearsIn", _.appearsIn),
            ValueField("friends", resolveFriends _),
          )
      ),
      ValueObjectMapping[Human](
        tpe = HumanType,
        fieldMappings =
          List(
            ValueField("homePlanet", _.homePlanet)
          )
      ),
      ValueObjectMapping[Droid](
        tpe = DroidType,
        fieldMappings =
          List(
            ValueField("primaryFunction", _.primaryFunction)
          )
      )
    )

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("hero", List(Binding("episode", TypedEnumValue(e))), child) =>
        val episode = Episode.values.find(_.toString == e.name).get
        Select("hero", Nil, Unique(Eql(FieldPath(List("id")), Const(hero(episode).id)), child)).rightIor
      case Select(f@("character" | "human" | "droid"), List(Binding("id", IDValue(id))), child) =>
        Select(f, Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)).rightIor
    }
  ))
}
