// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini
package grackle
package starwars

object StarWarsSchema {
  import Schema._, ScalarType._

  val EpisodeArg = InputValue("episode", None, TypeRef("Episode"), None)
  val IdArg = InputValue("id", None, NonNullType(StringType), None)

  val QueryType: ObjectType =
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("hero", None, List(EpisodeArg), NonNullType(TypeRef("Character")), false, None),
        Field("character", None, List(IdArg), TypeRef("Character"), false, None),
        Field("human", None, List(IdArg), TypeRef("Character"), false, None)
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

  val CharacterType: InterfaceType =
    InterfaceType(
      name = "Character",
      description = None,
      fields = List(
        Field("id", None, Nil, NonNullType(StringType), false, None),
        Field("name", None, Nil, StringType, false, None),
        Field("friends", None, Nil, ListType(TypeRef("Character")), false, None),
        Field("appearsIn", None, Nil, ListType(TypeRef("Episode")), false, None)
      )
    )

  val HumanType: ObjectType =
    ObjectType(
      name = "Human",
      description = None,
      fields = List(
        Field("id", None, Nil, NonNullType(StringType), false, None),
        Field("name", None, Nil, StringType, false, None),
        Field("friends", None, Nil, ListType(TypeRef("Character")), false, None),
        Field("appearsIn", None, Nil, ListType(TypeRef("Episode")), false, None),
        Field("homePlanet", None, Nil, StringType, false, None)
      ),
      interfaces = List(TypeRef("Character"))
    )

  val DroidType: ObjectType =
    ObjectType(
      name = "Droid",
      description = None,
      fields = List(
        Field("id", None, Nil, NonNullType(StringType), false, None),
        Field("name", None, Nil, StringType, false, None),
        Field("friends", None, Nil, ListType(TypeRef("Character")), false, None),
        Field("appearsIn", None, Nil, ListType(TypeRef("Episode")), false, None),
        Field("primaryFunction", None, Nil, StringType, false, None)
      ),
      interfaces = List(TypeRef("Character"))
    )

  val schema =
    Schema(
      types = List(QueryType, EpisodeType, CharacterType, HumanType, DroidType),
      queryType = TypeRef("Query"),
      mutationType = None,
      subscriptionType = None,
      directives = Nil
    )
}
