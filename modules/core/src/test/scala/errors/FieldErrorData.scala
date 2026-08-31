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

package errors

import cats.effect.IO

import grackle._
import grackle.Query._
import grackle.QueryCompiler._

/**
 * Mappings whose `name` field fails for one item of a list.
 *
 * The nullability of `name` and of the list decides where the null from that error lands.
 */
object FieldErrorMappings {

  case class Item(id: String, name: String)

  /**
   * The item whose name the mapping cannot fetch.
   */
  val failingId: String = "2"

  val message: String = s"Name for item $failingId could not be fetched."

  val items: List[Item] = List(Item("1", "one"), Item(failingId, "two"), Item("3", "three"))

  /**
   * `name` is nullable, so the null stays at the `name` position.
   */
  object NullableName extends ItemMapping(nullableName = true, nullableItems = true)

  /**
   * `name` is non-null, so the null bubbles up to the entry of the `items` list.
   */
  object NonNullName extends ItemMapping(nullableName = false, nullableItems = true)

  /**
   * No position between `name` and the root is nullable, so the null bubbles up to `data`.
   */
  object NonNullThroughout extends ItemMapping(nullableName = false, nullableItems = false)

  abstract class ItemMapping(nullableName: Boolean, nullableItems: Boolean)
      extends ValueMapping[IO] {

    private val nameTpe = if (nullableName) "String" else "String!"
    private val itemsTpe = if (nullableItems) "[Item]" else "[Item!]!"

    val schema: Schema =
      mkSchema(s"""
        type Query {
          ping: String
          items: $itemsTpe
        }
        type Item {
          id: ID!
          name: $nameTpe
        }
      """)

    val QueryType = schema.ref("Query")
    val ItemType = schema.ref("Item")

    private def itemName(c: Cursor): Result[String] =
      c.as[Item].flatMap { item =>
        if (item.id == failingId) Result.failure(message)
        else Result(item.name)
      }

    private val nameField: FieldMapping =
      if (nullableName) CursorField[Option[String]]("name", c => itemName(c).map(Some(_)))
      else CursorField[String]("name", itemName)

    private val itemsField: FieldMapping =
      if (nullableItems) ValueField[Unit]("items", _ => Some(items.map(Some(_))))
      else ValueField[Unit]("items", _ => items)

    val typeMappings =
      List(
        ValueObjectMapping[Unit](
          tpe = QueryType,
          fieldMappings = List(ValueField[Unit]("ping", _ => Some("pong")), itemsField)),
        ValueObjectMapping[Item](
          tpe = ItemType,
          fieldMappings = List(ValueField[Item]("id", _.id), nameField))
      )
  }

  /**
   * A mapping whose `name` field yields its value beside a warning.
   *
   * The warning is raised at the `name` position, so it carries the path of that position.
   */
  object WarningName extends ValueMapping[IO] {
    val schema: Schema =
      mkSchema("""
        type Query {
          items: [Item!]
        }
        type Item {
          id: ID!
          name: String
        }
      """)

    val QueryType = schema.ref("Query")
    val ItemType = schema.ref("Item")

    private def itemName(c: Cursor): Result[Option[String]] =
      c.as[Item].flatMap { item =>
        if (item.id == failingId) Result.warning(message, Some(item.name))
        else Result(Some(item.name))
      }

    val typeMappings =
      List(
        ValueObjectMapping[Unit](
          tpe = QueryType,
          fieldMappings = List(ValueField[Unit]("items", _ => Some(items)))),
        ValueObjectMapping[Item](
          tpe = ItemType,
          fieldMappings =
            List(ValueField[Item]("id", _.id), CursorField[Option[String]]("name", itemName))
        )
      )
  }

  /**
   * A mapping whose count field counts a field which fails.
   *
   * `tagCount` is nullable, so the null stays at the position of the count field.
   */
  object FailingCount extends ValueMapping[IO] {
    val schema: Schema =
      mkSchema("""
        type Query {
          ping: String
          tags: [String!]
          tagCount: Int
        }
      """)

    val QueryType = schema.ref("Query")

    val typeMappings =
      List(
        ValueObjectMapping[Unit](
          tpe = QueryType,
          fieldMappings = List(
            ValueField[Unit]("ping", _ => Some("pong")),
            CursorField[Option[List[String]]]("tags", _ => Result.failure(message)),
            ValueField[Unit]("tagCount", _ => 0)
          )
        )
      )

    override val selectElaborator = SelectElaborator {
      case (QueryType, "tagCount", _) =>
        Elab.transformChild(_ => Count(Select("tags")))
    }
  }

  /**
   * The component of the mappings which delegate. Its `name` field fails.
   */
  object FailingComponent extends ValueMapping[IO] {
    val schema: Schema =
      mkSchema("""
        type Query {
          ping: String
          delegated: Item!
        }
        type Item {
          name: String!
        }
      """)

    val QueryType = schema.ref("Query")
    val ItemType = schema.ref("Item")

    val typeMappings =
      List(
        ValueObjectMapping[Unit](
          tpe = QueryType,
          fieldMappings = List(
            ValueField[Unit]("ping", _ => Some("pong")),
            ValueField[Unit]("delegated", _ => Item(failingId, "two"))
          )
        ),
        ValueObjectMapping[Item](
          tpe = ItemType,
          fieldMappings = List(CursorField[String]("name", _ => Result.failure(message))))
      )
  }

  /**
   * `delegated` is nullable, so the null stays at the position of the delegated field.
   */
  object NullableDelegate extends DelegateMapping(nullableDelegate = true)

  /**
   * `delegated` is non-null, so the null bubbles up to `data`.
   */
  object NonNullDelegate extends DelegateMapping(nullableDelegate = false)

  /**
   * A mapping which delegates both of its fields to [[FailingComponent]].
   *
   * Both fields go into one batch, so the batch mixes a failed member with a successful one.
   */
  abstract class DelegateMapping(nullableDelegate: Boolean) extends ComposedMapping[IO] {

    private val delegatedTpe = if (nullableDelegate) "Item" else "Item!"

    val schema: Schema =
      mkSchema(s"""
        type Query {
          ping: String
          delegated: $delegatedTpe
        }
        type Item {
          name: String!
        }
      """)

    val QueryType = schema.ref("Query")

    val typeMappings =
      List(
        ObjectMapping(
          tpe = QueryType,
          fieldMappings = List(
            Delegate("ping", FailingComponent),
            Delegate("delegated", FailingComponent)
          )
        )
      )
  }

  /**
   * Builds a schema from `text`, or throws when `text` is not a valid schema.
   */
  private def mkSchema(text: String): Schema =
    Schema(text) match {
      case Result.Success(s) => s
      case Result.Warning(_, s) => s
      case other => throw new IllegalArgumentException(other.toProblems.toList.mkString("; "))
    }
}
