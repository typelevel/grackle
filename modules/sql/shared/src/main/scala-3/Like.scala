// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package sql

import scala.util.matching.Regex

case class Like(x: Term[String]|Term[Option[String]], pattern: String, caseInsensitive: Boolean) extends Predicate {
  lazy val r = Like.likeToRegex(pattern, caseInsensitive)
  def apply(c: Cursor): Result[Boolean] =
    x(c).map(_ match {
      case s: String => r.matches(s)
      case Some(s: String) => r.matches(s)
      case None => false
    })
  def children = List(x)
}

object Like {
  private def likeToRegex(pattern: String, caseInsensitive: Boolean): Regex = {
    val csr = ("^"+pattern.replace("%", ".*").replace("_", ".")+"$")
    (if (caseInsensitive) s"(?i:$csr)" else csr).r
  }
}
