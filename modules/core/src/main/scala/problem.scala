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

import cats.Eq
import io.circe._
import io.circe.syntax._

/** A problem, to be reported back to the user. */
final case class Problem(
  message: String,
  locations: List[(Int, Int)] = Nil,
  path: List[String] = Nil,
  extensions: Option[JsonObject] = None,
) {
  override def toString = {

    lazy val pathText: String =
      path.mkString("/")

    lazy val locationsText: String =
      locations.map { case (a, b) =>
        if (a == b) a.toString else s"$a..$b"
      } .mkString(", ")

    val s = (path.nonEmpty, locations.nonEmpty) match {
      case (true, true)   => s"$message (at $pathText: $locationsText)"
      case (true, false)  => s"$message (at $pathText)"
      case (false, true)  => s"$message (at $locationsText)"
      case (false, false) => message
    }

    extensions.fold(s)(obj => s"$s, extensions: ${obj.asJson.spaces2}")

  }

}

object Problem {

  implicit val ProblemEncoder: Encoder[Problem] = { p =>

    val locationsField: List[(String, Json)] =
      if (p.locations.isEmpty) Nil
      else List(
        "locations" ->
          p.locations.map { case (line, col) =>
            Json.obj(
              "line" -> line.asJson,
              "col"  -> col.asJson
            )
          } .asJson
      )

    val pathField: List[(String, Json)] =
      if (p.path.isEmpty) Nil
      else List(("path" -> p.path.asJson))

    val extensionsField: List[(String, Json)] =
      p.extensions.fold(List.empty[(String, Json)])(obj => List("extensions" -> obj.asJson))

    Json.fromFields(
      "message" -> p.message.asJson ::
      locationsField                :::
      pathField                     :::
      extensionsField
    )

  }

  implicit val eqProblem: Eq[Problem] =
    Eq.by(p => (p.message, p.locations, p.path))
}
