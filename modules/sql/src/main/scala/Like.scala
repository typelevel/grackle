package edu.gemini.grackle
package sql

import scala.util.matching.Regex


case class Like(x: Term[String], pattern: String, caseInsensitive: Boolean) extends Predicate {
  lazy val r = Like.likeToRegex(pattern, caseInsensitive)
  def apply(c: Cursor): Result[Boolean] = x(c).map(r.matches(_))
}

object Like {
  private def likeToRegex(pattern: String, caseInsensitive: Boolean): Regex = {
    val csr = ("^"+pattern.replace("%", ".*").replace("_", ".")+"$")
    (if (caseInsensitive) s"(?i:$csr)" else csr).r
  }
}