// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.Monad
import cats.implicits._
import io.circe.Json

import QueryInterpreter.{ mkErrorResult, ProtoJson }

abstract class DataTypeQueryInterpreter[F[_]: Monad](schema: Schema)
  extends QueryInterpreter[F](schema) {

  def rootCursor(query: Query): Result[(Type, Cursor)]

  def runRootValue(query: Query): F[Result[ProtoJson]] =
    (for {
      root          <- rootCursor(query)
      (tpe, cursor) =  root
      value         <- runValue(query, tpe, cursor)
    } yield value).pure[F]
}

trait DataTypeCursor extends Cursor {
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
          case None => mkErrorResult(s"Unrepresentable double %d")
        }
      case b: Boolean => Json.fromBoolean(b).rightIor
      case e: Enumeration#Value => Json.fromString(e.toString).rightIor
      case _ => mkErrorResult("Not a leaf")
    }
  }

  def isList: Boolean = focus match {
    case _: List[_] => true
    case _ => false
  }

  def asList: Result[List[Cursor]] = focus match {
    case it: List[_] => it.map(mkCursor).rightIor
    case _ => mkErrorResult("Not a list")
  }

  def isNullable: Boolean = focus match {
    case _: Option[_] => true
    case _ => false
  }

  def asNullable: Result[Option[Cursor]] = focus match {
    case o: Option[_] => o.map(mkCursor).rightIor
    case _ => mkErrorResult("Not nullable")
  }

  def hasAttribute(attributeName: String): Boolean = false

  def attribute(attributeName: String): Result[Any] =
    mkErrorResult(s"No attribute $attributeName")
}
