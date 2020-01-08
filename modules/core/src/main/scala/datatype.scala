// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.Monad
import cats.implicits._
import io.circe.Json

import Query.{ Select, Wrap }
import QueryInterpreter.{ mkErrorResult, ProtoJson }
import ScalarType._

/**
 * An interpreter of GraphQL queries relative to a model backed by an
 * in-memory Scala data type.
 *
 * The interpreter is parameterized with,
 *
 * 1. `root`: a `PartialFunction` from the top-level field name of the
 *    query to an initial GraphQL type and Scala value representing the
 *    starting point in the model for a `Cursor`-based traversal of the
 *    model to evaluate that query.
 *
 * 2. `fields`: a `PartialFunction` from a Scala value and a GraphQL
 *    field name to a Scala value. The argument value is the focus
 *    element in the model and corresponds to the GraphQL type at the
 *    current `Cursor` position. The GraphQL field name represents an
 *    edge out of that node, and the resulting Scala value corresponds
 *    to the GraphQL value of the field.
 *
 * 3. `attrs`: a `PartialFunction` from a Scala value and an attribute
 *    name to a Scala value. The argument value is the focus element in
 *    the model and corresponds to the GraphQL type at the current
 *    `Cursor` position. The attribute name selects an attribute value
 *    from that node, which is returned as the result.
 */
class DataTypeQueryInterpreter[F[_]: Monad](
  root:    PartialFunction[String, (Type, Any)],
  fields:  PartialFunction[(Any, String), Any],
  attrs:   PartialFunction[(Any, String), Any] = PartialFunction.empty,
  narrows: PartialFunction[(Any, Type), Any] = PartialFunction.empty
) extends QueryInterpreter[F] {

  def runRootValue(query: Query, rootTpe: Type): F[Result[ProtoJson]] =
    query match {
      case Select(fieldName, _, child) =>
        if (root.isDefinedAt(fieldName)) {
          val (tpe, focus) = root(fieldName)
          val cursor = DataTypeCursor(tpe, focus, fields, attrs, narrows)
          runValue(Wrap(fieldName, child), rootTpe.field(fieldName), cursor).pure[F]
        } else
          mkErrorResult(s"No root field '$fieldName'").pure[F]
      case _ =>
        mkErrorResult(s"Bad root query '${query.render}' in DataTypeQueryInterpreter").pure[F]
    }
}

/**
 * A `Cursor` for a `DataTypeQueryInterpreter` backed by a Scala data type.
 */
case class DataTypeCursor(
  tpe:     Type,
  focus:   Any,
  fields:  PartialFunction[(Any, String), Any],
  attrs:   PartialFunction[(Any, String), Any],
  narrows: PartialFunction[(Any, Type), Any]
) extends Cursor {
  def isLeaf: Boolean = (tpe.dealias, focus) match {
    case (_: ScalarType, (_ : String | _ : Int | _ : Double | _ : Boolean | _ : Enumeration#Value)) => true
    case _ => false
  }

  def asLeaf: Result[Json] = (tpe.dealias, focus) match {
    case (StringType,  s: String)  => Json.fromString(s).rightIor
    case (IntType,     i: Int)     => Json.fromInt(i).rightIor
    case (FloatType,   d: Double)  => Json.fromDouble(d) match {
        case Some(j) => j.rightIor
        case None => mkErrorResult(s"Unrepresentable double %d")
      }
    case (BooleanType, b: Boolean) => Json.fromBoolean(b).rightIor
    case (_: EnumType, e: Enumeration#Value) => Json.fromString(e.toString).rightIor
    case _ => mkErrorResult(s"Expected Scalar type, found $tpe for focus ${focus}")
  }

  def isList: Boolean = (tpe, focus) match {
    case (_: ListType, _: List[_]) => true
    case _ => false
  }

  def asList: Result[List[Cursor]] = (tpe, focus) match {
    case (ListType(tpe), it: List[_]) => it.map(f => copy(tpe = tpe, focus = f)).rightIor
    case _ => mkErrorResult(s"Expected List type, found $tpe")
  }

  def isNullable: Boolean = (tpe, focus) match {
    case (_: NullableType, _: Option[_]) => true
    case _ => false
  }

  def asNullable: Result[Option[Cursor]] = (tpe, focus) match {
    case (NullableType(tpe), o: Option[_]) => o.map(f => copy(tpe = tpe, focus = f)).rightIor
    case _ => mkErrorResult(s"Expected Nullable type, found $tpe")
  }

  def narrowsTo(subtpe: Type): Boolean =
    subtpe <:< tpe && narrows.isDefinedAt((focus, subtpe.dealias))

  def narrow(subtpe: Type): Result[Cursor] =
    if (narrowsTo(subtpe))
      copy(tpe = subtpe, focus = narrows((focus, subtpe.dealias))).rightIor
    else
      mkErrorResult(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")

  def hasField(fieldName: String): Boolean =
    tpe.hasField(fieldName) && fields.isDefinedAt((focus, fieldName))

  def field(fieldName: String): Result[Cursor] =
    if (hasField(fieldName))
      copy(tpe = tpe.field(fieldName), focus = fields((focus, fieldName))).rightIor
    else
      mkErrorResult(s"No field '$fieldName' for type $tpe")

  def hasAttribute(attributeName: String): Boolean =
    attrs.isDefinedAt((focus, attributeName))

  def attribute(attributeName: String): Result[Any] =
    if (hasAttribute(attributeName))
      attrs((focus, attributeName)).rightIor
    else
      mkErrorResult(s"No attribute '$attributeName' for type $tpe")
}
