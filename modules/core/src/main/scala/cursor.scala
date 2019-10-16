// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.data.Validated

import cats.Id
import cats.data.Validated, Validated.{ Valid, Invalid }
import cats.implicits._
import io.circe.Json

trait Cursor {
  def isLeaf: Boolean
  def asLeaf: Result[Json]
  def isList: Boolean
  def asList: Result[List[Cursor]]
  def isNullable: Boolean
  def asNullable: Result[Option[Cursor]]
  def hasField(field: String): Boolean
  def field(field: String, args: Map[String, Any]): Result[Cursor]
}

class CursorQueryInterpreter extends QueryInterpreter[Id] {
  import Query._
  import QueryInterpreter.mkError

  type Field = (String, Json)

  def runFields(q: Query, tpe: Type, cursor: Cursor): Result[List[Field]] = {
    (q, tpe) match {
      case (sel@Select(fieldName, _, _), NullableType(tpe)) =>
        cursor.asNullable.flatMap { (oc: Option[Cursor]) =>
          oc.map(c => runFields(sel, tpe, c)).getOrElse(List((fieldName, Json.Null)).rightIor)
        }

      case (Select(fieldName, bindings, child), tpe) =>
        cursor.field(fieldName, Binding.toMap(bindings)).flatMap { (c: Cursor) =>
          runValue(child, tpe.field(fieldName), c).map(value => List((fieldName, value)))
        }

      case (Group(siblings), _) =>
        siblings.flatTraverse(q => runFields(q, tpe, cursor))

      case _ =>
        List(mkError(s"failed: $q $tpe")).leftIor
    }
  }

  def runValue(q: Query, tpe: Type, cursor: Cursor): Result[Json] = {
    tpe match {
      case NullableType(tpe) =>
        cursor.asNullable.flatMap { (oc: Option[Cursor]) =>
          oc.map(c => runValue(q, tpe, c)).getOrElse(Json.Null.rightIor)
        }

      case ListType(tpe) =>
        cursor.asList.flatMap { (lc: List[Cursor]) =>
          lc.traverse(c => runValue(q, tpe, c)).map { (values: List[Json]) =>
            Json.fromValues(values)
          }
        }

      case TypeRef(schema, tpnme) =>
        schema.types.find(_.name == tpnme).map(tpe =>
          runValue(q, tpe, cursor)
        ).getOrElse(List(mkError(s"Unknown type '$tpnme'")).leftIor)

      case (_: ScalarType) | (_: EnumType) => cursor.asLeaf

      case (_: ObjectType) | (_: InterfaceType) =>
        runFields(q, tpe, cursor).map { (fields: List[Field]) =>
          Json.fromFields(fields)
        }

      case _ =>
        List(mkError(s"Unsupported type $tpe")).leftIor
    }
  }
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
