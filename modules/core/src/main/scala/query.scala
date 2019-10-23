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

  def runFields(query: Query, tpe: Type, cursor: Cursor): F[Result[List[(String, ProtoJson)]]] = {
    (query, tpe) match {
      case (sel@Select(fieldName, _, _), NullableType(tpe)) =>
        cursor.asNullable.flatTraverse(oc =>
          oc.map(c => runFields(sel, tpe, c)).getOrElse(List((fieldName, ProtoJson.fromJson(Json.Null))).rightIor.pure[F])
        )

      case (Select(fieldName, bindings, child), tpe) =>
        if (!cursor.hasField(fieldName)) {
          composedMapping.objectMappings.find(_.tpe == tpe) match {
            case Some(om) => om.fieldMappings.find(_._1 == fieldName) match {
              case Some((_, so: composedMapping.Subobject[t])) =>
                  List((fieldName, ProtoJson.deferred(so.subquery(cursor.focus.asInstanceOf[t], child), so.submapping.interpreter))).rightIor.pure[F]
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

  def runValue(query: Query, tpe: Type, cursor: Cursor): F[Result[ProtoJson]] = {
    tpe match {
      case NullableType(tpe) =>
        cursor.asNullable.flatTraverse(oc =>
          oc.map(c => runValue(query, tpe, c)).getOrElse(ProtoJson.fromJson(Json.Null).rightIor.pure[F])
        )

      case ListType(tpe) =>
        cursor.asList.flatTraverse(lc =>
          lc.traverse(c => runValue(query, tpe, c).nested).map(ProtoJson.fromValues).value
        )

      case TypeRef(schema, tpnme) =>
        schema.types.find(_.name == tpnme)
          .map(tpe => runValue(query, tpe, cursor))
          .getOrElse(List(mkError(s"Unknown type '$tpnme'")).leftIor.pure[F])

      case (_: ScalarType) | (_: EnumType) => cursor.asLeaf.map(ProtoJson.fromJson).pure[F]

      case (_: ObjectType) | (_: InterfaceType) =>
        runFields(query, tpe, cursor).nested.map(ProtoJson.fromFields).value

      case _ =>
        Thread.dumpStack
        List(mkError(s"Unsupported type $tpe")).leftIor.pure[F]
    }
  }

  sealed trait ProtoJson {
    def run: F[Result[Json]]
  }

  object ProtoJson {
    case class PureJson(value: Json) extends ProtoJson {
      def run: F[Result[Json]] = value.rightIor.pure[F]
    }

    case class DeferredJson(query: Query, interpreter: QueryInterpreter[F]) extends ProtoJson {
      def run: F[Result[Json]] = interpreter.runRootValue(query)
    }

    case class ProtoObject(fields: List[(String, ProtoJson)]) extends ProtoJson {
      def run: F[Result[Json]] =
        (fields.traverse { case (name, value) => value.run.nested.map(v => (name, v)) }).map(Json.fromFields).value
    }

    case class ProtoArray(elems: List[ProtoJson]) extends ProtoJson {
      def run: F[Result[Json]] =
        elems.traverse(_.run.nested).map(Json.fromValues).value
    }

    def deferred(query: Query, interpreter: QueryInterpreter[F]): ProtoJson = DeferredJson(query, interpreter)

    def fromJson(value: Json): ProtoJson = PureJson(value)

    def fromFields(fields: List[(String, ProtoJson)]): ProtoJson =
      if(fields.forall(_._2.isInstanceOf[PureJson]))
        PureJson(Json.fromFields(fields.map { case (name, c) => (name, c.asInstanceOf[PureJson].value) }))
      else
        ProtoObject(fields)

    def fromValues(elems: List[ProtoJson]): ProtoJson =
      if(elems.forall(_.isInstanceOf[PureJson]))
        PureJson(Json.fromValues(elems.map(_.asInstanceOf[PureJson].value)))
      else
        ProtoArray(elems)
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
