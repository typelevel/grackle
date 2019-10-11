// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.data.Validated

import cats.Id
import cats.data.Validated, Validated.{ Valid, Invalid }
import cats.implicits._
import io.circe.Json

trait Cursor {
  type Result[T] = Validated[String, T]

  def isLeaf: Boolean
  def asLeaf: Result[Json]
  def isList: Boolean
  def asList: Result[List[Cursor]]
  def isNullable: Boolean
  def asNullable: Result[Option[Cursor]]
  def hasField(field: String): Boolean
  def field(field: String, args: Map[String, Any]): Result[Cursor]
}

class CursorQueryInterpreter extends QueryInterpreter[Id, Json] {
  import Query._

  type Result[T] = Validated[String, T]
  type Field = (String, Json)

  def runFields(q: Query, tpe: Type, cursor: Cursor): Result[List[Field]] = {
    (q, tpe) match {
      case (sel@Select(fieldName, _, _), NullableType(tpe)) =>
        cursor.asNullable.andThen { (oc: Option[Cursor]) =>
          oc.map(c => runFields(sel, tpe, c)).getOrElse(Valid(List((fieldName, Json.Null))))
        }

      case (Select(fieldName, bindings, child), tpe) =>
        cursor.field(fieldName, Binding.toMap(bindings)).andThen { (c: Cursor) =>
          runValue(child, tpe.field(fieldName), c).map(value => List((fieldName, value)))
        }

      case (Group(siblings), _) =>
        siblings.flatTraverse(q => runFields(q, tpe, cursor))

      case _ =>
        Invalid(s"failed: $q $tpe")
    }
  }

  def runValue(q: Query, tpe: Type, cursor: Cursor): Result[Json] = {
    tpe match {
      case NullableType(tpe) =>
        cursor.asNullable.andThen { (oc: Option[Cursor]) =>
          oc.map(c => runValue(q, tpe, c)).getOrElse(Valid(Json.Null))
        }

      case ListType(tpe) =>
        cursor.asList.andThen { (lc: List[Cursor]) =>
          lc.traverse(c => runValue(q, tpe, c)).map { (values: List[Json]) =>
            Json.fromValues(values)
          }
        }

      case TypeRef(schema, tpnme) =>
        schema.types.find(_.name == tpnme).map(tpe =>
          runValue(q, tpe, cursor)
        ).getOrElse(Invalid(s"Unknown type '$tpnme'"))

      case (_: ScalarType) | (_: EnumType) => cursor.asLeaf

      case (_: ObjectType) | (_: InterfaceType) =>
        runFields(q, tpe, cursor).map { (fields: List[Field]) =>
          Json.fromFields(fields)
        }

      case _ =>
        Invalid(s"Unsupported type $tpe")
    }
  }
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
