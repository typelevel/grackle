// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package demo.starwars

import cats.syntax.all._
import grackle.Predicate._
import grackle.Query._
import grackle.QueryCompiler._
import grackle.Value._
import grackle._
import grackle.generic._
import grackle.syntax._

trait StarWarsMapping[F[_]] extends GenericMapping[F] { self: StarWarsData[F] =>
  // #schema
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
  // #schema

  val QueryType = schema.ref("Query")
  val EpisodeType = schema.ref("Episode")
  val CharacterType = schema.ref("Character")
  val HumanType = schema.ref("Human")
  val DroidType = schema.ref("Droid")

  val typeMappings =
    List(
      // #root
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
      // #root
    )

  // #elaborator
  override val selectElaborator = SelectElaborator {
    // The hero selector takes an Episode argument and yields a single value. We transform
    // the nested child to use the Filter and Unique operators to pick out the target using
    // the Eql predicate.
    case (QueryType, "hero", List(Binding("episode", EnumValue(e)))) =>
      for {
        episode <- Elab.liftR(Episode.values.find(_.toString == e).toResult(s"Unknown episode '$e'"))
        _       <- Elab.transformChild(child => Unique(Filter(Eql(CharacterType / "id", Const(hero(episode).id)), child)))
      } yield ()

    // The character, human and droid selectors all take a single ID argument and yield a
    // single value (if any) or null. We transform the nested child to use the Unique and
    // Filter operators to pick out the target using the Eql predicate.
    case (QueryType, "character" | "human" | "droid", List(Binding("id", IDValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CharacterType / "id", Const(id)), child)))
  }
  // #elaborator
}

// The types and values for the in-memory Star Wars example.
trait StarWarsData[F[_]] extends GenericMapping[F] { self: StarWarsMapping[F] =>
  import semiauto._

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
      deriveInterfaceCursorBuilder[Character](CharacterType)
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

  def resolveFriends(c: Character): Result[Option[List[Character]]] =
    c.friends match {
      case None => None.success
      case Some(ids) =>
        ids.traverse(id => characters.find(_.id == id).toResultOrError(s"Bad id '$id'")).map(_.some)
    }

  // #model_types

  // #model_values
  // The character database ...
  lazy val characters: List[Character] = List(
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

  val Some(lukeSkywalker) = characters.find(_.id == "1000") : @unchecked
  val Some(r2d2) = characters.find(_.id == "2001") : @unchecked

  // Mapping from Episode to its hero Character
  val hero: Map[Value, Character] = Map(
    NEWHOPE -> r2d2,
    EMPIRE -> lukeSkywalker,
    JEDI -> r2d2
  )
}
