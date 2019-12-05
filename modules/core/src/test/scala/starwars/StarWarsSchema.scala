// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package starwars

import edu.gemini.grackle._

object StarWarsSchema extends Schema {
  import ScalarType._

  val EpisodeArg = InputValue("episode", None, NullableType(TypeRef("Episode")), None)
  val IdArg = InputValue("id", None, StringType, None)

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("hero", None, List(EpisodeArg), TypeRef("Character"), false, None),
        Field("character", None, List(IdArg), NullableType(TypeRef("Character")), false, None),
        Field("human", None, List(IdArg), NullableType(TypeRef("Character")), false, None)
      ),
      interfaces = Nil
    ),
    EnumType(
      name = "Episode",
      description = None,
      enumValues = List(
        EnumValue("NEWHOPE", None),
        EnumValue("EMPIRE", None),
        EnumValue("JEDI", None)
      )
    ),
    InterfaceType(
      name = "Character",
      description = None,
      fields = List(
        Field("id", None, Nil, StringType, false, None),
        Field("name", None, Nil, NullableType(StringType), false, None),
        Field("friends", None, Nil, NullableType(ListType(TypeRef("Character"))), false, None),
        Field("appearsIn", None, Nil, NullableType(ListType(TypeRef("Episode"))), false, None)
      )
    ),
    ObjectType(
      name = "Human",
      description = None,
      fields = List(
        Field("id", None, Nil, StringType, false, None),
        Field("name", None, Nil, NullableType(StringType), false, None),
        Field("friends", None, Nil, NullableType(ListType(TypeRef("Character"))), false, None),
        Field("appearsIn", None, Nil, NullableType(ListType(TypeRef("Episode"))), false, None),
        Field("homePlanet", None, Nil, NullableType(StringType), false, None)
      ),
      interfaces = List(TypeRef("Character"))
    ),
    ObjectType(
      name = "Droid",
      description = None,
      fields = List(
        Field("id", None, Nil, StringType, false, None),
        Field("name", None, Nil, NullableType(StringType), false, None),
        Field("friends", None, Nil, NullableType(ListType(TypeRef("Character"))), false, None),
        Field("appearsIn", None, Nil, NullableType(ListType(TypeRef("Episode"))), false, None),
        Field("primaryFunction", None, Nil, NullableType(StringType), false, None)
      ),
      interfaces = List(TypeRef("Character"))
    )
  )

  val directives = Nil
}
