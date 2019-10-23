// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.Applicative
import cats.implicits._
import io.circe.Json
import io.circe.literal.JsonStringContext

trait QueryInterpreter[F[_]] {
  import Query._
  import QueryInterpreter.mkError

  sealed trait DJson {
    def run: F[Result[Json]]
  }

  object DJson {
    case class CJson(value: Json) extends DJson {
      def run: F[Result[Json]] = value.rightIor.pure[F]
    }

    case class PJson(query: Query, interpreter: QueryInterpreter[F]) extends DJson {
      def run: F[Result[Json]] = interpreter.runRootValue(query)
    }

    case class DObj(fields: List[(String, DJson)]) extends DJson {
      def run: F[Result[Json]] =
        (fields.traverse { case (name, value) => value.run.nested.map(v => (name, v)) }).map(Json.fromFields).value
    }

    case class DArray(elems: List[DJson]) extends DJson {
      def run: F[Result[Json]] =
        elems.traverse(_.run.nested).map(Json.fromValues).value
    }

    def partial(query: Query, interpreter: QueryInterpreter[F]): DJson = PJson(query, interpreter)

    def fromJson(value: Json): DJson = CJson(value)

    def fromFields(fields: List[(String, DJson)]): DJson =
      if(fields.forall(_._2.isInstanceOf[CJson]))
        CJson(Json.fromFields(fields.map { case (name, c) => (name, c.asInstanceOf[CJson].value) }))
      else
        DObj(fields)

    def fromValues(elems: List[DJson]): DJson =
      if(elems.forall(_.isInstanceOf[CJson]))
        CJson(Json.fromValues(elems.map(_.asInstanceOf[CJson].value)))
      else
        DArray(elems)
  }

  val schema: Schema
  val composedMapping: Mapping[F]

  implicit val F: Applicative[F]

  def run(query: Query): F[Json] =
    runRoot(query).map(QueryInterpreter.mkResponse)

  def runRoot(query: Query): F[Result[Json]] =
    query match {
      case Select(fieldName, _, _) =>
        runRootValue(query).nested.map(value => Json.obj((fieldName, value))).value
      case _ =>
        List(mkError(s"Bad query: $query")).leftIor.pure[F]
    }

  def runRootValue(query: Query): F[Result[Json]]

  def runFields(query: Query, tpe: Type, cursor: Cursor): F[Result[List[(String, DJson)]]] = {
    (query, tpe) match {
      case (sel@Select(fieldName, _, _), NullableType(tpe)) =>
        cursor.asNullable.flatTraverse(oc =>
          oc.map(c => runFields(sel, tpe, c)).getOrElse(List((fieldName, DJson.fromJson(Json.Null))).rightIor.pure[F])
        )

      case (Select(fieldName, bindings, child), tpe) =>
        if (!cursor.hasField(fieldName)) {
          composedMapping.objectMappings.find(_.tpe == tpe) match {
            case Some(om) => om.fieldMappings.find(_._1 == fieldName) match {
              case Some((_, so: composedMapping.Subobject[t])) =>
                  List((fieldName, DJson.partial(so.subquery(cursor.focus.asInstanceOf[t], child), so.submapping.interpreter))).rightIor.pure[F]
                case _ => List(mkError(s"failed: $query $tpe")).leftIor.pure[F]
              }
            case _ => List(mkError(s"failed: $query $tpe")).leftIor.pure[F]
          }
        } else
          cursor.field(fieldName, Binding.toMap(bindings)).flatTraverse(c =>
            runValue(child, tpe.field(fieldName), c).nested.map(value => List((fieldName, value))).value
          )

      case (Group(siblings), _) =>
        siblings.flatTraverse(query => runFields(query, tpe, cursor).nested).value

      case _ =>
        List(mkError(s"failed: $query $tpe")).leftIor.pure[F]
    }
  }

  def runValue(query: Query, tpe: Type, cursor: Cursor): F[Result[DJson]] = {
    tpe match {
      case NullableType(tpe) =>
        cursor.asNullable.flatTraverse(oc =>
          oc.map(c => runValue(query, tpe, c)).getOrElse(DJson.fromJson(Json.Null).rightIor.pure[F])
        )

      case ListType(tpe) =>
        cursor.asList.flatTraverse(lc =>
          lc.traverse(c => runValue(query, tpe, c).nested).map(DJson.fromValues).value
        )

      case TypeRef(schema, tpnme) =>
        schema.types.find(_.name == tpnme)
          .map(tpe => runValue(query, tpe, cursor))
          .getOrElse(List(mkError(s"Unknown type '$tpnme'")).leftIor.pure[F])

      case (_: ScalarType) | (_: EnumType) => cursor.asLeaf.map(DJson.fromJson).pure[F]

      case (_: ObjectType) | (_: InterfaceType) =>
        runFields(query, tpe, cursor).nested.map(DJson.fromFields).value

      case _ =>
        Thread.dumpStack
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

sealed trait Query {
  import Query._

  def ~(query: Query): Query = (this, query) match {
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
}
