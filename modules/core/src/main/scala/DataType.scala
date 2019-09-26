// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.data.Validated, Validated.{ Valid, Invalid }
import io.circe.Json

trait DataTypeCursor extends Cursor {
  val focus: Any
  def mkCursor(focus: Any): Cursor

  def isLeaf: Boolean = focus match {
    case (_ : String | _ : Int | _ : Double | _ : Boolean | _ : Enumeration#Value) => true
    case _ => false
  }

  def asLeaf: Result[Json] = {
    focus match {
      case s: String => Valid(Json.fromString(s))
      case i: Int => Valid(Json.fromInt(i))
      case d: Double => Validated.fromOption(Json.fromDouble(d), s"Unrepresentable double %d")
      case b: Boolean => Valid(Json.fromBoolean(b))
      case e: Enumeration#Value => Valid(Json.fromString(e.toString))
      case _ => Invalid("Not a leaf")
    }
  }

  def isList: Boolean = focus match {
    case _: List[_] => true
    case _ => false
  }

  def asList: Result[List[Cursor]] = focus match {
    case it: List[_] => Valid(it.map(mkCursor))
    case _ => Invalid("Not a list")
  }

  def isNullable: Boolean = focus match {
    case _: Option[_] => true
    case _ => false
  }

  def asNullable: Result[Option[Cursor]] = focus match {
    case o: Option[_] => Valid(o.map(mkCursor))
    case _ => Invalid("Not nullable")
  }
}
