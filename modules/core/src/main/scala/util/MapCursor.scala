// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package util

import io.circe.Json

/**
 * A cursor that contains a named map of cursor-constructing functions.
 * @param fields a map from field name to a function to construct a cursor for that field, given
 *   a parent cursor (this one) and a new context, which will extend `context` as determined by
 *   the field name.
 */
case class MapCursor(
  context: Cursor.Context,
  parent:  Option[Cursor] = None,
  env:     Cursor.Env = Cursor.Env.EmptyEnv,
  fields:  Map[String, (Cursor, Cursor.Context) => Cursor] = Map.empty,
) extends Cursor {

  def focus: Any = null
  def asLeaf: Result[Json] = Result.failure(s"MapCursor: $tpe is not a leaf type.")
  def asList: Result[List[Cursor]] = Result.failure(s"MapCursor: $tpe is not a list type.")
  def asNullable: Result[Option[Cursor]] = Result.failure(s"MapCursor: $tpe is not a nullable type.")
  def isLeaf: Boolean = false
  def isList: Boolean = false
  def isNullable: Boolean = false
  def narrow(subtpe: TypeRef): Result[Cursor] = Result.failure(s"MapCursor: narrowing is unsupported.")
  def narrowsTo(subtpe: TypeRef): Boolean = false
  def withEnv(env: Cursor.Env): Cursor = copy(env = env)
  def hasField(fieldName: String): Boolean = fields.contains(fieldName)

  def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
    fields.get(fieldName) match {
      case None    => Result.failure(s"MapCursor: $tpe has no mapping for field $fieldName.")
      case Some(mkChild) =>
        Result.fromOption(
          context.forField(fieldName, resultName).map(mkChild(this, _)),
          s"MapCursor: $tpe has a mapping for field $fieldName but no such field exists on the schema type."
        )
    }

  def withField(name: String, value: Int): MapCursor =
    withField(name, (p, c) => ScalarCursor(Some(p), env, c, value))

  def withField(name: String, value: String): MapCursor =
    withField(name, (p, c) => ScalarCursor(Some(p), env, c, value))

  def withField(name: String, value: Boolean): MapCursor =
    withField(name, (p, c) => ScalarCursor(Some(p), env, c, value))

  def withField(name: String, mkChild: (Cursor, Cursor.Context) => Cursor): MapCursor =
    copy(fields = fields + (name -> mkChild))

  /**
   * Map a field to a list of cursors, which will be passed a context for the underlying (non-array)
   * type.
   */
  def withListField(name: String, mkChild: (Cursor, Cursor.Context) => List[Cursor]): MapCursor =
    withField(name, (p, c) => ListCursor(Some(p), c, mkChild(p, c.asType(c.tpe.underlying))))

}

