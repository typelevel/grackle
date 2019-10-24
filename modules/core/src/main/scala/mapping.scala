// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.implicits._
import io.circe.Json

import Query._
import QueryInterpreter.{ mkError, ProtoJson }

trait ComponentMapping[F[_]] {
  val objectMappings: List[ObjectMapping]

  def subobject(tpe: Type, fieldName: String): Option[Subobject] =
    objectMappings.find(_.tpe =:= tpe) match {
      case Some(om) =>
        om.fieldMappings.find(_._1 == fieldName) match {
          case Some((_, so: Subobject)) => Some(so)
          case _ => None
        }
      case None => None
    }

  sealed trait FieldMapping

  case class ObjectMapping(
    tpe: Type,
    interpreter: QueryInterpreter[F],
    fieldMappings: List[(String, FieldMapping)]
  )

  val defaultJoin: (Cursor, Query) => Query = (_, subquery: Query) => subquery

  case class Subobject(
    submapping: ObjectMapping,
    subquery: (Cursor, Query) => Query = defaultJoin
  ) extends FieldMapping
}

object ComponentMapping {
  def NoMapping[F[_]]: ComponentMapping[F] =
    new ComponentMapping[F] {
      val objectMappings = Nil
    }
}

trait ComposedQueryInterpreter[F[_]] extends QueryInterpreter[F] with ComponentMapping[F] {
  def run(query: Query): F[Json] = run(query, this)

  def runRootValue(query: Query): F[Result[ProtoJson]] = {
    query match {
      case Select(fieldName, _, _) =>
        subobject(schema.queryType, fieldName) match {
          case Some(Subobject(submapping, _)) => submapping.interpreter.runRootValue(query)
          case None => List(mkError("Bad query")).leftIor.pure[F]
        }
      case _ => List(mkError("Bad query")).leftIor.pure[F]
    }
  }
}
