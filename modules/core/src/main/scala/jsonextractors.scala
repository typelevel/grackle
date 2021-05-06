// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import io.circe.Json
import io.circe.JsonObject

object JsonExtractor {

  object jsonNull {
    def unapply(j: Json): Option[Unit] = j.asNull
  }

  object jsonBoolean {
    def unapply(j: Json): Option[Boolean] = j.asBoolean
  }

  object jsonString {
    def unapply(j: Json): Option[String] = j.asString
  }

  object jsonInt {
    def unapply(j: Json): Option[Int] = j.asNumber.flatMap(_.toInt)
  }

  object jsonDouble {
    def unapply(j: Json): Option[Double] = j.asNumber.map(_.toDouble)
  }

  object jsonArray {
    def unapply(j: Json): Option[Vector[Json]] = j.asArray
  }

  object jsonObject {
    def unapply(j: Json): Option[JsonObject] = j.asObject
  }

}