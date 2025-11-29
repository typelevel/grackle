// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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
