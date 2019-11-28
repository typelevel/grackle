// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.Monad
import cats.implicits._
import io.circe.Json

import Query.Wrap
import QueryInterpreter.{ mkErrorResult, ProtoJson }
import ScalarType._

class DataTypeQueryInterpreter[F[_]: Monad](
  root:   PartialFunction[String, (Type, Any)],
  fields: PartialFunction[(Any, String), Any],
  attrs:  PartialFunction[(Any, String), Any] = PartialFunction.empty
) extends QueryInterpreter[F] {

  def runRootValue(query: Query, rootTpe: Type): F[Result[ProtoJson]] =
    query match {
      case Wrap(fieldName, _) =>
        if (root.isDefinedAt(fieldName)) {
          val (tpe, focus) = root(fieldName)
          val cursor = DataTypeCursor(tpe, focus, fields, attrs)
          runValue(query, rootTpe.field(fieldName), cursor).pure[F]
        } else
          mkErrorResult(s"No root field '$fieldName'").pure[F]
      case _ =>
        mkErrorResult(s"Bad root query '${query.render}' in DataTypeQueryInterpreter").pure[F]
    }
}

case class DataTypeCursor(
  tpe:    Type,
  focus:  Any,
  fields: PartialFunction[(Any, String), Any],
  attrs:  PartialFunction[(Any, String), Any]
) extends Cursor {
  def isLeaf: Boolean = (tpe, focus) match {
    case (_: ScalarType, (_ : String | _ : Int | _ : Double | _ : Boolean | _ : Enumeration#Value)) => true
    case _ => false
  }

  def asLeaf: Result[Json] = (tpe, focus) match {
    case (StringType,  s: String)  => Json.fromString(s).rightIor
    case (IntType,     i: Int)     => Json.fromInt(i).rightIor
    case (FloatType,   d: Double)  => Json.fromDouble(d) match {
        case Some(j) => j.rightIor
        case None => mkErrorResult(s"Unrepresentable double %d")
      }
    case (BooleanType, b: Boolean) => Json.fromBoolean(b).rightIor
    case (_: EnumType, e: Enumeration#Value) => Json.fromString(e.toString).rightIor
    case _ => mkErrorResult(s"Expected Scalar type, found ${tpe.shortString} for focus ${focus}")
  }

  def isList: Boolean = (tpe, focus) match {
    case (_: ListType, _: List[_]) => true
    case _ => false
  }

  def asList: Result[List[Cursor]] = (tpe, focus) match {
    case (ListType(tpe), it: List[_]) => it.map(f => copy(tpe = tpe, focus = f)).rightIor
    case _ => mkErrorResult(s"Expected List type, found ${tpe.shortString}")
  }

  def isNullable: Boolean = focus match {
    case (_: NullableType, _: Option[_]) => true
    case _ => false
  }

  def asNullable: Result[Option[Cursor]] = (tpe, focus) match {
    case (NullableType(tpe), o: Option[_]) => o.map(f => copy(tpe = tpe, focus = f)).rightIor
    case _ => mkErrorResult(s"Expected Nullable type, found ${tpe.shortString}")
  }

  def hasField(fieldName: String): Boolean =
    tpe.hasField(fieldName) && fields.isDefinedAt((focus, fieldName))

  def field(fieldName: String, args: Map[String, Any]): Result[Cursor] =
    if (hasField(fieldName))
      copy(tpe = tpe.field(fieldName), focus = fields((focus, fieldName))).rightIor
    else
      mkErrorResult(s"No field '$fieldName' for type ${tpe.shortString}")

  def hasAttribute(attributeName: String): Boolean =
    attrs.isDefinedAt((focus, attributeName))

  def attribute(attributeName: String): Result[Any] =
    if (hasAttribute(attributeName))
      attrs((focus, attributeName)).rightIor
    else
      mkErrorResult(s"No attribute '$attributeName' for type ${tpe.shortString}")
}
