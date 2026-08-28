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
 * A column, and how the dialects disagree about writing its values, if they do.
 */
case class Column(name: String, kind: Kind, sqlType: String)

object Column {

  /**
   * `nextshowing:timestamptz` is a timestamp column; a bare `title` is a plain one. The type
   * comes from the dialect's own schema, which is where Oracle's array constructor lives.
   */
  def parse(header: String, sqlTypeOf: String => String): Column = {
    val (name, kind) = header.split(":", -1) match {
      case Array(name) => (name, Kind.Plain: Kind)
      case Array(name, kind) => (name, Kind.named(kind))
      case _ => sys.error(s"malformed column header '$header'")
    }
    Column(name, kind, sqlTypeOf(name))
  }
}
