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
}

sealed trait Query {
  import Query._

  def ~(q: Query): Query = (this, q) match {
    case (Group(hd), Group(tl)) => Group(hd ++ tl)
    case (hd, Group(tl)) => Group(hd :: tl)
    case (Group(hd), tl) => Group(hd :+ tl)
    case (hd, tl) => Group(List(hd, tl))
  }

  def /(q: Query): Query = Nest(this, q)
}

object Query {
  case class Select(name: String, args: List[Binding]) extends Query
  case class Group(queries: List[Query]) extends Query
  case class Nest(parent: Query, child: Query) extends Query
}
