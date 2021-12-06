// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package util

import io.circe.{ Json, Encoder }

/** A cursor that contains a single scalar value. */
case class ScalarCursor[A: Encoder](
  val parent:  Option[Cursor],
  val env:     Cursor.Env,
  val context: Cursor.Context,
  val focus:   A,
) extends Cursor {
  def asLeaf: Result[Json] = Result(Encoder[A].apply(focus))
  def asList: Result[List[Cursor]] = Result.failure(s"ScalarCursor: $tpe is not a list type.")
  def asNullable: Result[Option[Cursor]] = Result.failure(s"ScalarCursor: $tpe is not a nullable type.")
  def field(fieldName: String, resultName: Option[String]): Result[Cursor] = Result.failure(s"ScalarCursor: $tpe has no fields.")
  def hasField(fieldName: String): Boolean = false
  def isLeaf: Boolean = true
  def isList: Boolean = false
  def isNullable: Boolean = false
  def narrow(subtpe: TypeRef): Result[Cursor] =
    if (narrowsTo(subtpe)) Result(withTpe(subtpe))
    else Result.failure(s"ScalarCursor: cannot narrow $tpe to $subtpe.")
  def narrowsTo(subtpe: TypeRef): Boolean = subtpe <:< tpe
  def withTpe(tpe: Type): Cursor = copy(context = context.asType(tpe))
  def withEnv(env: Cursor.Env): Cursor = copy(env = env)
}

