package edu.gemini.grackle.sql

/** A database row is a list of untyped values (some of which may be `FailedJoin`). */
case class Row(elems: List[Any]) {
  def apply(i: Int): Any = elems(i)
}
