// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package sql

import scala.util.matching.Regex

import syntax._

case class Like private[sql] (x: Term[_], pattern: String, caseInsensitive: Boolean) extends Predicate {
  lazy val r = Like.likeToRegex(pattern, caseInsensitive)
  def apply(c: Cursor): Result[Boolean] =
    x(c).flatMap(_ match {
      case s: String => r.matches(s).success
      case Some(s: String) => r.matches(s).success
      case None => false.success
      case other => Result.internalError(s"Expected value of type String or Option[String], found $other")
    })
  def children = List(x)
}

object Like extends Like0 {
  private[sql] def apply(x: Term[_], pattern: String, caseInsensitive: Boolean): Like =
    new Like(x, pattern, caseInsensitive)

  private def likeToRegex(pattern: String, caseInsensitive: Boolean): Regex = {
    val csr = ("^"+pattern.replace("%", ".*").replace("_", ".")+"$")
    (if (caseInsensitive) s"(?i:$csr)" else csr).r
  }
}

trait Like0 {
  trait PossiblyOptionString[T]
  object PossiblyOptionString extends PossiblyOptionString0 {
    implicit val sInst: PossiblyOptionString[String] = new PossiblyOptionString[String] {}
  }
  trait PossiblyOptionString0 {
    implicit val osInst: PossiblyOptionString[Option[String]] = new PossiblyOptionString[Option[String]] {}
  }

  def apply[T](x: Term[T], pattern: String, caseInsensitive: Boolean)(implicit @annotation.nowarn ev: PossiblyOptionString[T]): Predicate =
    new Like(x, pattern, caseInsensitive)
}
