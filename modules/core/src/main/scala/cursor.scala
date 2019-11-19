// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import io.circe.Json

trait Cursor {
  def focus: Any
  def isLeaf: Boolean
  def asLeaf: Result[Json]
  def isList: Boolean
  def asList: Result[List[Cursor]]
  def isNullable: Boolean
  def asNullable: Result[Option[Cursor]]
  def hasField(fieldName: String): Boolean
  def field(fieldName: String, args: Map[String, Any]): Result[Cursor]
  def hasAttribute(attributeName: String): Boolean
  def attribute(attributeName: String): Result[Any]
}
