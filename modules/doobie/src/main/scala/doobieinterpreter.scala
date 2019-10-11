// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package doobie

import cats.data.Validated, Validated.{ Valid, Invalid }
import cats.effect.Bracket
import cats.implicits._
import _root_.doobie.{ Fragment, Transactor }
import _root_.doobie.implicits._
import io.chrisdavenport.log4cats.Logger
import io.circe.Json

import DoobieMapping._

abstract class DoobieQueryInterpreter[F[_]] extends QueryInterpreter[F, Json] {
  val mapping: DoobieMapping
  val xa: Transactor[F]
  val logger: Logger[F]
  implicit val brkt: Bracket[F, Throwable]

  type Result[T] = Validated[String, T]

  def run[T](q: Query, tpe: Type): F[Result[Json]]

  def runRoot(query: Query, tpe: Type, fieldName: String, predicates: List[Fragment]): F[Result[Json]] = {
    val fieldTpe = tpe.field(fieldName)
    val mapped = mapping.mapQuery(query, fieldTpe, predicates)
    val interpreter = new CursorQueryInterpreter

    for {
      table <- logger.info(s"fetch(${mapped.fragment})") *> mapped.fetch.transact(xa)
      value  = interpreter.runValue(query, fieldTpe, DoobieCursor(fieldTpe, table, mapped))
    } yield
      value.map(value => Json.obj(fieldName -> value))
  }
}

case class DoobieCursor(val tpe: Type, val focus: Any, mapped: MappedQuery) extends Cursor {
  def asTable: Result[Table] = focus match {
    case table: List[_] => Valid(table.asInstanceOf[Table])
    case _ => Invalid(s"Not a table")
  }

  def isLeaf: Boolean = tpe.isLeaf

  def asLeaf: Result[Json] =
    focus match {
      case s: String => Valid(Json.fromString(s))
      case i: Int => Valid(Json.fromInt(i))
      case d: Double => Validated.fromOption(Json.fromDouble(d), s"Unrepresentable double %d")
      case b: Boolean => Valid(Json.fromBoolean(b))
      case _ => Invalid("Not a leaf")
    }

  def isList: Boolean =
    tpe match {
      case ListType(_) => true
      case _ => false
    }

  def asList: Result[List[Cursor]] =
    if (!tpe.isList) Invalid(s"Not a list: $tpe")
    else {
      val itemTpe = tpe.item.dealias
      asTable.map(table => mapped.group(table, itemTpe).map(table => copy(tpe = itemTpe, focus = table)))
    }

  def isNullable: Boolean =
    tpe match {
      case NullableType(_) => true
      case _ => false
    }

  def asNullable: Result[Option[Cursor]] =
    (tpe, focus) match {
      case (NullableType(_), None) => Valid(None)
      case (NullableType(tpe), Some(v)) => Valid(Some(copy(tpe = tpe, focus = v)))
      case (NullableType(_), null) => Valid(None)
      case (NullableType(tpe), v) => Valid(Some(copy(tpe = tpe, focus = v)))
      case _ => Invalid("Not nullable")
    }

  def hasField(field: String): Boolean =
    tpe.field(field) != NoType

  def field(field: String, args: Map[String, Any]): Result[Cursor] = {
    val fieldTpe = tpe.dealias.field(field)
    if (fieldTpe.isLeaf)
      asTable.map(table => copy(tpe = fieldTpe, focus = mapped.select(table.head, tpe, field)))
    else
      Valid(copy(tpe = fieldTpe))
  }
}
