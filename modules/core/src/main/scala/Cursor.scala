// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.data.Validated

import cats.Id
import cats.data.Validated, Validated.{ Valid, Invalid }
import cats.implicits._
import io.circe.Json
import io.circe.literal.JsonStringContext

trait Cursor {
  type Result[T] = Validated[String, T]

  def isLeaf: Boolean
  def asLeaf: Result[Json]
  def isList: Boolean
  def asList: Result[List[Cursor]]
  def isNullable: Boolean
  def asNullable: Result[Option[Cursor]]
  def hasField(label: String): Boolean
  def field(label: String, args: Map[String, Any]): Result[Cursor]
}

trait CursorQueryInterpreter extends QueryInterpreter[Id, Json] {
  import Query._
  import Schema._
  import SchemaUtils._

  type Result[T] = Validated[String, T]
  type Field = (String, Json)

  val rootSchema: Schema
  val rootCursor: Cursor

  def run(q: Query): Json =
    run(q, rootSchema.queryType, rootCursor).map { fields =>
      Json.obj("data" -> Json.fromFields(fields))
    }.valueOr((err: String) => json""" { "errors": [ { "message": $err } ] } """)

  def hasField(tpe: Type, fieldName: String): Boolean =
    schemaOfField(rootSchema, tpe, fieldName).isDefined

  def field(tpe: Type, fieldName: String): Type =
    schemaOfField(rootSchema, tpe, fieldName).get

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

      case TypeRef(tpnme) =>
        rootSchema.types.find(_.name == tpnme).map(tpe =>
          runValue(q, tpe, cursor)
        ).getOrElse(Invalid(s"Unknown type '$tpnme'"))

      case (_: ScalarType) | (_: EnumType) => cursor.asLeaf

      case (_: ObjectType) | (_: InterfaceType) =>
        run(q, tpe, cursor).map { (fields: List[Field]) =>
          Json.fromFields(fields)
        }

      case _ =>
        Invalid(s"Unsupported type $tpe")
    }
  }

  def run(q: Query, tpe: Type, cursor: Cursor): Result[List[Field]] = {
    (q, tpe) match {
      case (sel@Select(fieldName, _), NullableType(tpe)) if hasField(tpe, fieldName) =>
        cursor.asNullable.andThen { (oc: Option[Cursor]) =>
          oc.map(c => run(sel, tpe, c)).getOrElse(Valid(List((fieldName, Json.Null))))
        }

      case (sel@Select(fieldName, bindings), tpe) if hasField(tpe, fieldName) =>
        cursor.field(fieldName, Binding.toMap(bindings)).andThen { (c: Cursor) =>
          runValue(sel.child, field(tpe, fieldName), c).map(value => List((fieldName, value)))
        }

      case (Group(siblings), _) =>
        siblings.flatTraverse(q => run(q, tpe, cursor))

      case _ =>
        Invalid(s"failed: $q $tpe")
    }
  }
}
