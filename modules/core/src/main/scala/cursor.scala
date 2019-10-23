// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.implicits._
import io.circe.Json

trait Cursor {
  def focus: Any
  def isLeaf: Boolean
  def asLeaf: Result[Json]
  def isList: Boolean
  def asList: Result[List[Cursor]]
  def isNullable: Boolean
  def asNullable: Result[Option[Cursor]]
  def hasField(field: String): Boolean
  def field(field: String, args: Map[String, Any]): Result[Cursor]
}

trait DataTypeCursor extends Cursor {
  import QueryInterpreter.mkError

  val focus: Any
  def mkCursor(focus: Any): Cursor

  def isLeaf: Boolean = focus match {
    case (_ : String | _ : Int | _ : Double | _ : Boolean | _ : Enumeration#Value) => true
    case _ => false
  }

  def asLeaf: Result[Json] = {
    focus match {
      case s: String => Json.fromString(s).rightIor
      case i: Int => Json.fromInt(i).rightIor
      case d: Double => Json.fromDouble(d) match {
          case Some(j) => j.rightIor
          case None => List(mkError(s"Unrepresentable double %d")).leftIor
        }
      case b: Boolean => Json.fromBoolean(b).rightIor
      case e: Enumeration#Value => Json.fromString(e.toString).rightIor
      case _ => List(mkError("Not a leaf")).leftIor
    }
  }

  def isList: Boolean = focus match {
    case _: List[_] => true
    case _ => false
  }

  def asList: Result[List[Cursor]] = focus match {
    case it: List[_] => it.map(mkCursor).rightIor
    case _ => List(mkError("Not a list")).leftIor
  }

  def isNullable: Boolean = focus match {
    case _: Option[_] => true
    case _ => false
  }

  def asNullable: Result[Option[Cursor]] = focus match {
    case o: Option[_] => o.map(mkCursor).rightIor
    case _ => List(mkError("Not nullable")).leftIor
  }
}
