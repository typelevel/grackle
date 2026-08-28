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

package grackle.sql.test

import cats.effect.IO
import munit.{CatsEffectSuite, Location}

import grackle._
import grackle.sql.FailedJoin

trait SqlErrorKeysSuite extends CatsEffectSuite {
  def mapping: SqlCompositeKeyMapping[IO]

  final lazy val M = mapping

  val query = "{ parents { key1 key2 children { id parent1 parent2 } } }"

  lazy val (mapped, rootContext) =
    (for {
      op <- M.compiler.compile(query)
      context = Context(op.rootTpe)
      mq <- M.MappedQuery(op.query, context)
      // The compiled query is never empty here, so the cast is safe.
    } yield (mq.asInstanceOf[M.MappedQuery.NonEmptyMappedQuery], context)) match {
      case Result.Success(res) => res
      case other => fail(s"Expected a non-empty mapped query, got: $other")
    }

  lazy val parentContext = itemContext(rootContext, "parents")
  lazy val childContext = itemContext(parentContext, "children")

  def itemContext(context: Context, fieldName: String): Context = {
    val fieldContext = context.forFieldOrAttribute(fieldName, None)
    val itemTpe =
      fieldContext.tpe.item.map(_.dealias).getOrElse(fail(s"Not a list: $fieldName"))
    fieldContext.asType(itemTpe)
  }

  def colIndex(context: Context, fieldName: String): Int =
    mapped.colsByResultPath(fieldName :: context.resultPath).head._2

  def mkRow(cells: (Int, Any)*): Array[Any] = {
    val row = Array.fill[Any](mapped.index.values.max + 1)(FailedJoin)
    cells.foreach { case (i, v) => row(i) = v }
    row
  }

  def mkTable(rows: Array[Any]*): M.Table = M.Table(rows.toVector)

  def errorMessage(res: Result[Any])(implicit loc: Location): String =
    res match {
      case Result.InternalError(err) => err.getMessage
      case other => fail(s"Expected an internal error, got: $other")
    }

  test("single value selects") {
    val id = colIndex(childContext, "id")
    val parent1 = colIndex(childContext, "parent1")
    val table = mkTable(mkRow(id -> 1, parent1 -> 10), mkRow(id -> 1, parent1 -> 10))
    val res: Result[Any] = mapped.selectAtomicField(childContext, "parent1", table)
    assertEquals(res, Result.Success[Any](10))
  }

  test("many values name the key") {
    val id = colIndex(childContext, "id")
    val parent1 = colIndex(childContext, "parent1")
    val table = mkTable(mkRow(id -> 1, parent1 -> 10), mkRow(id -> 1, parent1 -> 20))
    val msg = errorMessage(mapped.selectAtomicField(childContext, "parent1", table))
    assertNoDiff(
      msg,
      "Expected single value for field 'parent1' of type Child at List(children, parents), found many (keys: composite_key_child.id = 1)"
    )
  }

  test("string key renders quoted, and a key with many values renders as <many>") {
    val key1 = colIndex(parentContext, "key1")
    val key2 = colIndex(parentContext, "key2")
    val table = mkTable(mkRow(key1 -> 1, key2 -> "GBR"), mkRow(key1 -> 2, key2 -> "GBR"))
    val msg = errorMessage(mapped.selectAtomicField(parentContext, "key1", table))
    assertNoDiff(
      msg,
      "Expected single value for field 'key1' of type Parent at List(parents), found many (keys: composite_key_parent.key_1 = <many>, composite_key_parent.key_2 = \"GBR\")"
    )
  }
}
