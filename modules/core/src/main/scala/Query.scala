// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

trait QueryInterpreter[F[_], A] {
  def run(q: Query): F[A]
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
