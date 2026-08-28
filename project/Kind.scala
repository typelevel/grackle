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

/**
 * What a CSV column holds, where the dialects disagree about how to write it.
 */
sealed trait Kind

object Kind {
  case object Plain extends Kind
  case object Array extends Kind
  case object Date extends Kind
  case object Time extends Kind
  case object Timestamp extends Kind
  case object Boolean extends Kind

  /**
   * The whole vocabulary a CSV header can name. Anything else is a typo.
   */
  private val byName =
    Map[String, Kind](
      "array" -> Array,
      "date" -> Date,
      "time" -> Time,
      "timestamptz" -> Timestamp,
      "boolean" -> Boolean
    )

  def named(name: String): Kind =
    byName.getOrElse(name, sys.error(s"unknown column kind '$name'"))
}
