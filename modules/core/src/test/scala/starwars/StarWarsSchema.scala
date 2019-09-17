// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini
package grackle
package starwars

object StarWarsSchema {
  import Schema._, ScalarType._

  lazy val schema =
    Schema(
      queryType = QueryType,
      mutationType = None,
      subscriptionType = None,
      directives = Nil
    )

  val EpisodeArg = InputValue("episode", None, EpisodeType, None)
  val IdArg = InputValue("id", None, NonNullType(StringType), None)

  val QueryType: ObjectType =
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("hero", None, List(EpisodeArg), NonNullType(CharacterType), false, None),
        Field("character", None, List(IdArg), CharacterType, false, None),
        Field("human", None, List(IdArg), CharacterType, false, None)
      ),
      interfaces = Nil
    )

  val EpisodeType: EnumType =
    EnumType(
      name = "Episode",
      description = None,
      enumValues = List(
        EnumValue("NEWHOPE", None),
        EnumValue("EMPIRE", None),
        EnumValue("JEDI", None)
      )
    )

  lazy val CharacterType: InterfaceType = 
    InterfaceType(
      name = "Character",
      description = None,
      fields = List(
        Field("id", None, Nil, NonNullType(StringType), false, None),
        Field("name", None, Nil, StringType, false, None),
        Field("friends", None, Nil, ListType(CharacterType), false, None),
        Field("appearsIn", None, Nil, ListType(EpisodeType), false, None)
      )
    )

  lazy val HumanType: ObjectType = 
    ObjectType(
      name = "Human",
      description = None,
      fields = List(
        Field("id", None, Nil, NonNullType(StringType), false, None),
        Field("name", None, Nil, StringType, false, None),
        Field("friends", None, Nil, ListType(CharacterType), false, None),
        Field("appearsIn", None, Nil, ListType(EpisodeType), false, None),
        Field("homePlanet", None, Nil, StringType, false, None)
      ),
      interfaces = List(CharacterType)
    )

  lazy val DroidType: ObjectType = 
    ObjectType(
      name = "Droid",
      description = None,
      fields = List(
        Field("id", None, Nil, NonNullType(StringType), false, None),
        Field("name", None, Nil, StringType, false, None),
        Field("friends", None, Nil, ListType(CharacterType), false, None),
        Field("appearsIn", None, Nil, ListType(EpisodeType), false, None),
        Field("primaryFunction", None, Nil, StringType, false, None)
      ),
      interfaces = List(CharacterType)
    )
}
