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
 * A table a dialect's schema creates, and the type it declares for each of its columns.
 */
case class Table(name: String, sqlTypes: Map[String, String]) {

  /**
   * The CSV holding this table's rows. A table can be schema-qualified, and the qualifier is
   * the dialect's business, not the shared rows', so only the table's own name is used.
   */
  def csvName: String = name.substring(name.lastIndexOf('.') + 1)
}

object Table {

  /**
   * The tables a schema creates, in creation order. A table with a foreign key has to be
   * created after the table it references, so creation order is also a safe insertion order.
   *
   * A table ends at the first `)` that begins a line, indentation aside, which is how all three
   * dialects write one, with or without a trailing semicolon:
   *
   * {{{
   * CREATE TABLE bintree (               Table("bintree", Map("id" -> "INTEGER", ...))
   *   id INTEGER PRIMARY KEY,
   *   left_child INTEGER
   * );
   *
   * CREATE TABLE brands (                Table("brands", Map("id" -> "Int", ...)) — SQL
   *     id Int PRIMARY KEY,              Server's scripts end a table without a semicolon
   *     categories Int
   * )
   *
   * CREATE TABLE movies (                "categories" -> "string_array2", which is also
   *     categories string_array2,        the constructor Oracle builds the array with
   *     title VARCHAR(100) NOT NULL
   * );
   * }}}
   *
   * A `)` inside a column's own type doesn't end the table, because it has more than blank
   * space before it: `VARCHAR(100)`, `CHECK (ISJSON(x) = 1)`, `INTERVAL DAY (0) TO SECOND (0)`.
   * Anything that isn't a CREATE TABLE is skipped, which is what leaves Oracle's
   * `CREATE TYPE ... AS VARRAY(100) OF VARCHAR2(100)` and `CREATE SEQUENCE city_id` alone.
   *
   * A table can be schema-qualified, and the qualifier is kept, since the INSERT needs it:
   * `qualified.union_order_entities` on Postgres and SQL Server, `QUALIFIED.` on Oracle. The
   * CSV is looked up by the table's own name, which is the same for all three.
   */
  def parse(schema: String): List[Table] =
    """(?im)CREATE\s+TABLE\s+"?((?:[A-Za-z0-9_]+"?\."?)?[A-Za-z0-9_]+)"?\s*\(([\s\S]*?)^\s*\)"""
      .r
      .findAllMatchIn(schema)
      .map(m => Table(m.group(1), declaredTypes(m.group(2))))
      .toList

  /**
   * The type each column of a table is declared as, taken a line at a time. Oracle needs this
   * to write an array: a collection type's name is also its constructor.
   */
  private def declaredTypes(body: String): Map[String, String] =
    body
      .linesIterator
      .map(_.trim.stripSuffix(",").split("\\s+").toList)
      .collect {
        case name :: tpe :: _ if !Constraints.contains(name.toUpperCase) => (name, tpe)
      }
      .toMap

  private val Constraints = Set("PRIMARY", "FOREIGN", "CONSTRAINT", "UNIQUE", "CHECK")
}
