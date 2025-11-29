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
