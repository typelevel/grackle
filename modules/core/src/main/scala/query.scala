// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.data.Validated
import io.circe.Json
import io.circe.literal.JsonStringContext

trait QueryInterpreter[F[_]] {
  //def run(q: Query): F[A]
}

object QueryInterpreter {
  type Result[T] = Validated[String, T]

  def mkResponse(data: Option[Json], errors: List[Json] = Nil): Json = {
    val dataField = data.map { value => ("data", value) }.toList
    val errorField = if (errors.isEmpty) Nil else List(("errors", Json.fromValues(errors)))
    Json.fromFields(errorField ++ dataField)
  }

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

  def mkResponse(result: Result[Json]): Json =
    (for {
      data <- result
    } yield mkResponse(Some(data), Nil)).valueOr(msg => mkResponse(None, List(mkError(msg))))
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
