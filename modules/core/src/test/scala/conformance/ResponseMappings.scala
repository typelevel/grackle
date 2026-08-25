// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package conformance

import cats.effect.IO

import grackle._
import grackle.Predicate.{Const, Eql}
import grackle.Query.{Binding, Filter, Unique}
import grackle.QueryCompiler._
import grackle.Value.EnumValue

/**
 * Mappings for the examples of section 7, Response.
 *
 * Section 7.1.6, Errors, states that the name of the character with ID 1002 could not be
 * fetched. The `name` field of that one character therefore fails here, and every other field
 * resolves.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Response
 */
object ResponseMappings {

  case class Character(id: String, name: String, friendIds: List[String])

  val luke: Character = Character("1000", "Luke Skywalker", List("1003"))
  val han: Character = Character("1002", "Han Solo", Nil)
  val leia: Character = Character("1003", "Leia Organa", Nil)
  val r2d2: Character = Character("2001", "R2-D2", List("1000", "1002", "1003"))

  /**
   * The characters which the examples of section 7 name.
   */
  val characters: List[Character] = List(luke, han, leia, r2d2)

  val heroes: Map[String, Character] =
    Map("NEWHOPE" -> r2d2, "EMPIRE" -> luke, "JEDI" -> r2d2)

  /**
   * The character whose name the specification cannot fetch.
   */
  val unfetchableId: String = han.id

  val unfetchableMessage: String =
    s"Name for character with ID $unfetchableId could not be fetched."

  /**
   * The mapping whose `name` field is nullable, so an error leaves `null` in place.
   */
  object NullableName extends HeroMapping(nullableName = true)

  /**
   * The mapping whose `name` field is non-null, so an error bubbles up to the list entry.
   */
  object NonNullName extends HeroMapping(nullableName = false)

  /**
   * The schema and the data of section 7.
   *
   * Section 7.1.6 states one response for a nullable `name` and one for a non-null `name`, so
   * `nullableName` selects between the two forms.
   */
  abstract class HeroMapping(nullableName: Boolean) extends ValueMapping[IO] {
    val schema: Schema =
      ConformanceSuite.mkSchema(s"""
        type Query { hero(episode: Episode!): Character }
        enum Episode { NEWHOPE EMPIRE JEDI }
        type Character {
          id: ID!
          name: String${if (nullableName) "" else "!"}
          friends: [Character]
        }
      """)

    val QueryType = schema.ref("Query")
    val CharacterType = schema.ref("Character")

    private def heroName(c: Cursor): Result[String] =
      c.as[Character].flatMap { ch =>
        if (ch.id == unfetchableId) Result.failure(unfetchableMessage)
        else Result(ch.name)
      }

    private val nameField: FieldMapping =
      if (nullableName) CursorField[Option[String]]("name", c => heroName(c).map(Some(_)))
      else CursorField[String]("name", heroName)

    val typeMappings =
      List(
        ValueObjectMapping[Unit](
          tpe = QueryType,
          fieldMappings = List(ValueField("hero", _ => characters))),
        ValueObjectMapping[Character](
          tpe = CharacterType,
          fieldMappings = List(
            ValueField("id", _.id),
            nameField,
            ValueField(
              "friends",
              c => Some(c.friendIds.map(id => characters.find(_.id == id)))
            )
          )
        )
      )

    override val selectElaborator: SelectElaborator =
      SelectElaborator {
        case (QueryType, "hero", List(Binding("episode", EnumValue(e)))) =>
          Elab.transformChild(child =>
            Unique(Filter(Eql(CharacterType / "id", Const(heroes(e).id)), child)))
      }
  }
}
