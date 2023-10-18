// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package grackle
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
