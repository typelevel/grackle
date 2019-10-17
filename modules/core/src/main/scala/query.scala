// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.{ Applicative }
import cats.implicits._
import io.circe.Json
import io.circe.literal.JsonStringContext

trait QueryInterpreter[F[_]] {
  import Query._
  import QueryInterpreter.mkError

  implicit val F: Applicative[F]

  def run(q: Query): F[Json]

  def runFields(q: Query, tpe: Type, cursor: Cursor): F[Result[List[(String, Json)]]] = {
    (q, tpe) match {
      case (sel@Select(fieldName, _, _), NullableType(tpe)) =>
        cursor.asNullable.flatTraverse(oc =>
          oc.map(c => runFields(sel, tpe, c)).getOrElse(List((fieldName, Json.Null)).rightIor.pure[F])
        )

      case (Select(fieldName, bindings, child), tpe) =>
        cursor.field(fieldName, Binding.toMap(bindings)).flatTraverse(c =>
          runValue(child, tpe.field(fieldName), c).nested.map(value => List((fieldName, value))).value
        )

      case (Group(siblings), _) =>
        siblings.flatTraverse(q => runFields(q, tpe, cursor).nested).value

      case _ =>
        List(mkError(s"failed: $q $tpe")).leftIor.pure[F]
    }
  }

  def runValue(q: Query, tpe: Type, cursor: Cursor): F[Result[Json]] = {
    tpe match {
      case NullableType(tpe) =>
        cursor.asNullable.flatTraverse(oc =>
          oc.map(c => runValue(q, tpe, c)).getOrElse(Json.Null.rightIor.pure[F])
        )

      case ListType(tpe) =>
        cursor.asList.flatTraverse(lc =>
          lc.traverse(c => runValue(q, tpe, c)).map(_.sequence.map(Json.fromValues))
        )

      case TypeRef(schema, tpnme) =>
        schema.types.find(_.name == tpnme)
          .map(tpe => runValue(q, tpe, cursor))
          .getOrElse(List(mkError(s"Unknown type '$tpnme'")).leftIor.pure[F])

      case (_: ScalarType) | (_: EnumType) => cursor.asLeaf.pure[F]

      case (_: ObjectType) | (_: InterfaceType) =>
        runFields(q, tpe, cursor).nested.map(Json.fromFields).value

      case _ =>
        List(mkError(s"Unsupported type $tpe")).leftIor.pure[F]
    }
  }
}

object QueryInterpreter {
  def mkResponse(data: Option[Json], errors: List[Json] = Nil): Json = {
    val dataField = data.map { value => ("data", value) }.toList
    val errorField = if (errors.isEmpty) Nil else List(("errors", Json.fromValues(errors)))
    Json.fromFields(errorField ++ dataField)
  }

  def mkResponse(result: Result[Json]): Json =
    mkResponse(result.right, result.left.getOrElse(Nil))

  def mkError(message: String, locations: List[(Int, Int)] = Nil, path: List[String] = Nil): Json = {
    val locationsField =
      if (locations.isEmpty) Nil
      else
        List((
          "locations",
          Json.fromValues(locations.map { case (line, col) => json""" { "line": $line, "col": $col } """ })
        ))
    val pathField =
      if (path.isEmpty) Nil
      else List(("path", Json.fromValues(path.map(Json.fromString))))

    Json.fromFields(("message", Json.fromString(message)) :: locationsField ++ pathField)
  }
}

sealed trait Binding {
  def name: String
  type T
  val value: T
}
object Binding {
  case class StringBinding(name: String, value: String) extends Binding { type T = String }

  def toMap(bindings: List[Binding]): Map[String, Any] =
    bindings.map(b => (b.name, b.value)).toMap
}

sealed trait Query {
  import Query._

  def ~(q: Query): Query = (this, q) match {
    case (Group(hd), Group(tl)) => Group(hd ++ tl)
    case (hd, Group(tl)) => Group(hd :: tl)
    case (Group(hd), tl) => Group(hd :+ tl)
    case (hd, tl) => Group(List(hd, tl))
  }
}

object Query {
  case class Select(name: String, args: List[Binding], child: Query = Empty) extends Query
  case class Group(queries: List[Query]) extends Query
  case object Empty extends Query
}
