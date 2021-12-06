// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package util

import io.circe.Json

/** A cursor that contains a list of cursors. */
case class ListCursor(
  parent:  Option[Cursor],
  context: Cursor.Context,
  focus:   List[Cursor],
  env:     Cursor.Env = Cursor.Env.EmptyEnv,
) extends Cursor {
  def asLeaf: Result[Json] = Result.failure(s"ListCursor: not a leaf")
  def asList: Result[List[Cursor]] = Result(focus)
  def asNullable: Result[Option[Cursor]] = Result.failure(s"ListCursor: not nullable")
  def field(fieldName: String, resultName: Option[String]): Result[Cursor] = Result.failure(s"ListCursor: no fields")
  def hasField(fieldName: String): Boolean = false
  def isLeaf: Boolean = false
  def isList: Boolean = true
  def isNullable: Boolean = false
  def narrow(subtpe: TypeRef): Result[Cursor] = Result.failure(s"ListCursor: can't narrow")
  def narrowsTo(subtpe: TypeRef): Boolean = false
  def withEnv(env: Cursor.Env): Cursor = copy(env = env)
}

