// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

trait Mapping[F[_]] {
  val objectMappings: List[ObjectMapping]

  sealed trait FieldMapping

  case class ObjectMapping(
    tpe: Type,
    interpreter: QueryInterpreter[F],
    fieldMappings: List[(String, FieldMapping)]
  )

  val defaultJoin: (Any, Query) => Query = (_, subquery: Query) => subquery

  case class Subobject[T](
    submapping: ObjectMapping,
    subquery: (T, Query) => Query = defaultJoin
  ) extends FieldMapping
}

object NoMapping {
  def apply[F[_]]: Mapping[F] =
    new Mapping[F] {
      val objectMappings = Nil
    }
}
