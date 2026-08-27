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

import Script._
import fs2.{Fallible, Pipe, Stream}
import fs2.data.csv.{lowlevel, CsvRow}

/**
 * One table's rows, as the INSERT statements a single dialect reads.
 */
case class Script(table: Table, dialect: Dialect) {

  def statements(csv: String): String =
    Stream
      .emit(csv)
      .through(lowlevel.rows[Fallible, String]('|'))
      .through(lowlevel.headers[Fallible, String])
      .through(inserts)
      .compile
      .toList
      .fold(throw _, _.mkString)

  /**
   * Turns a stream of rows into one INSERT statement per [[Script.ChunkSize]] of them.
   */
  private def inserts: Pipe[Fallible, CsvRow[String], String] =
    _.chunkN(ChunkSize, allowFewer = true).map { chunk =>
      val rows = chunk.toList
      val columns = columnsOf(rows.head)
      val names = columns.map(_.name).mkString(", ")
      val tuples = rows.map(tuple(columns, _)).mkString(",\n")
      s"INSERT INTO ${table.name} ($names) VALUES\n$tuples${dialect.terminator}\n"
    }

  /**
   * The columns the CSV header names, each with the type this dialect declares for it.
   */
  private def columnsOf(row: CsvRow[String]): List[Column] = {
    // A column the schema doesn't declare means the CSV and that dialect have drifted apart.
    def sqlTypeOf(column: String): String =
      table
        .sqlTypes
        .getOrElse(
          column,
          sys.error(
            s"${dialect.name}: the schema for ${table.name} declares no column $column"))
    row.headers.get.toList.map(Column.parse(_, sqlTypeOf))
  }

  /**
   * One row, as the `('a', 'b')` a VALUES clause takes.
   */
  private def tuple(columns: List[Column], row: CsvRow[String]): String =
    columns
      .zip(row.values.toList)
      .map { case (column, cell) => dialect.value(column, cell) }
      .mkString("(", ", ", ")")
}

object Script {

  /**
   * Rows per INSERT. SQL Server caps a multi-row VALUES clause at 1000.
   */
  private val ChunkSize = 500
}
