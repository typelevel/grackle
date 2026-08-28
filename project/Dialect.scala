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

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

import Dialect._
import fs2.{Fallible, Stream}
import fs2.data.csv.lowlevel

/**
 * How one database spells the values the dialects disagree about.
 */
sealed abstract class Dialect(val name: String) {
  def terminator: String = ";"
  def date(value: String): String = literal(value)
  def time(value: String): String = literal(value)
  def timestamp(value: String): String = literal(value)
  def boolean(value: String): String = literal(value.toUpperCase)
  def array(elements: List[String], sqlType: String): String

  /**
   * `\N` is the CSV's null; a plain value is a string literal for the database to coerce.
   */
  final def value(column: Column, cell: String): String =
    if (cell == "\\N") "NULL"
    else
      column.kind match {
        case Kind.Plain => literal(cell)
        case Kind.Array => array(elements(cell), column.sqlType)
        case Kind.Date => date(cell)
        case Kind.Time => time(cell)
        case Kind.Timestamp => timestamp(cell)
        case Kind.Boolean => boolean(cell)
      }

  final def literal(value: String): String = s"'${value.replace("'", "''")}'"
}

object Postgres extends Dialect("pg") {
  def array(elements: List[String], sqlType: String): String =
    literal(elements.map(quoted).mkString("{", ",", "}"))
}

object Oracle extends Dialect("oracle") {
  override def date(value: String): String = s"DATE ${literal(value)}"
  override def time(value: String): String = s"INTERVAL '0 $value' DAY TO SECOND (0)"
  override def timestamp(value: String): String = s"TIMESTAMP ${literal(sqlTimestamp(value))}"

  /**
   * A VARRAY value is built by calling the type, so the column's type is the constructor.
   */
  def array(elements: List[String], sqlType: String): String = {
    require(sqlType.nonEmpty, "an array column needs a collection type in Oracle's schema")
    elements.map(literal).mkString(s"$sqlType(", ", ", ")")
  }
}

object SqlServer extends Dialect("mssql") {
  override def terminator: String = ";\nGO"
  override def timestamp(value: String): String = literal(sqlTimestamp(value))
  override def boolean(value: String): String = if (value.toBoolean) "1" else "0"

  /**
   * SQL Server has no array type; the mappings read a JSON array out of a string column.
   */
  def array(elements: List[String], sqlType: String): String =
    literal(elements.map(quoted).mkString("[", ", ", "]"))
}

object Dialect {

  /**
   * An array's elements are comma separated, quoted the way any other CSV field would be.
   */
  def elements(cell: String): List[String] =
    if (cell.isEmpty) Nil
    else
      Stream
        .emit(cell)
        .through(lowlevel.rows[Fallible, String](','))
        .compile
        .toList
        .fold(throw _, _.head.values.toList)

  def quoted(element: String): String =
    "\"" + element.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  /**
   * ISO-8601 in the CSV; `2020-05-22 19:35:00 +00:00` is what Oracle and SQL Server read.
   */
  def sqlTimestamp(value: String): String =
    OffsetDateTime.parse(value).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss xxx"))
}
