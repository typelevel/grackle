// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import io.circe._
import io.circe.syntax._

/** A problem, to be reported back to the user. */
final case class Problem(
  message: String,
  locations: List[(Int, Int)] = Nil,
  path: List[String] = Nil,
  extension: Option[JsonObject] = None,
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

    extension.fold(s)(obj => s"$s, extension: ${obj.asJson.spaces2}")

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

    val extensionField: List[(String, Json)] =
      p.extension.fold(List.empty[(String, Json)])(obj => List("extension" -> obj.asJson))

    Json.fromFields(
      "message" -> p.message.asJson ::
      locationsField                :::
      pathField                     :::
      extensionField
    )

  }

}