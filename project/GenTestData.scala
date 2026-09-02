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

import sbt._
import sbt.io.IO

/**
 * Builds the database initialisation scripts the test containers are seeded from.
 *
 * A dataset is one directory under `testdata`, holding a schema per dialect as `<dialect>.sql`
 * and its rows once, as `<table>.csv`. This writes the two together into
 * `target/testdata/<dialect>/<dataset>.sql`, which is what docker compose mounts into the
 * container's init directory. A dataset with no CSVs — one whose data is still dialect-specific
 * — is copied through untouched, and one with no `<dialect>.sql` is skipped for that dialect.
 *
 * Rows are rendered as ordinary INSERT statements. Most values are written as a string literal
 * and coerced by the database, which is how the hand-written per-dialect scripts already
 * spelled them, so the CSV usually says nothing about types. The exceptions are the values the
 * dialects spell differently — arrays, temporals, booleans — where a column names its kind in
 * the CSV header (`categories:array`) and each `Dialect` renders it its own way.
 */
object GenTestData {

  private val Dialects = List(Postgres, Oracle, SqlServer, Sqlite, H2, MySql)

  def apply(baseDir: File): Unit = {
    val datasets = IO.listFiles(baseDir / "testdata").filter(_.isDirectory)
    Dialects.foreach { dialect =>
      val to = baseDir / "target" / "testdata" / dialect.name
      IO.createDirectory(to)
      val written =
        datasets.flatMap { dataset =>
          val schemaFile = dataset / s"${dialect.name}.sql"
          if (!schemaFile.exists) None
          else {
            val schema = IO.read(schemaFile)
            val script = to / s"${dataset.getName}.sql"
            IO.write(script, schema + renderDataset(dataset, schema, dialect))
            Some(script.getName)
          }
        }.toSet
      // Drop scripts for datasets that no longer exist, so a stale file can't seed a container.
      IO.listFiles(to).filter(f => !written.contains(f.getName)).foreach(IO.delete)
    }
  }

  /**
   * Renders every dataset without writing anything, and reports what failed rather than
   * stopping at the first problem. Needs no database, so it can run wherever the build does.
   */
  def check(baseDir: File): List[String] = {
    val datasets = IO.listFiles(baseDir / "testdata").filter(_.isDirectory).sortBy(_.getName)
    val problems =
      datasets.toList.flatMap { dataset =>
        val schemas = Dialects.filter(d => (dataset / s"${d.name}.sql").exists)
        if (schemas.isEmpty) List(s"${dataset.getName}: no <dialect>.sql at all")
        else
          schemas.flatMap { dialect =>
            try {
              renderDataset(dataset, IO.read(dataset / s"${dialect.name}.sql"), dialect)
              Nil
            } catch {
              case e: Throwable =>
                val why = e
                  .getMessage
                  .stripPrefix("requirement failed: ")
                  .stripPrefix(s"${dataset.getName}: ")
                List(s"${dataset.getName} (${dialect.name}): $why")
            }
          }
      }
    val tables = datasets.map(d => IO.listFiles(d).count(_.getName.endsWith(".csv"))).sum
    println(s"${datasets.length} datasets, $tables shared tables, ${Dialects.length} dialects")
    problems
  }

  /**
   * The INSERTs for one dataset, in the order its tables are created.
   */
  private def renderDataset(dataset: File, schema: String, dialect: Dialect): String = {
    val created = Table.parse(schema)
    // A CSV whose table this schema never creates would otherwise be silently left out.
    val rows = IO.listFiles(dataset).filter(_.getName.endsWith(".csv"))
    val unseeded = rows.map(_.getName.stripSuffix(".csv")).toSet -- created.map(_.csvName).toSet
    require(
      unseeded.isEmpty,
      s"${dataset.getName}: ${unseeded.mkString(", ")} not created by the ${dialect.name} schema")
    val statements =
      created.map(table => (table, dataset / s"${table.csvName}.csv")).collect {
        case (table, csv) if csv.exists =>
          Script(table, dialect).statements(IO.read(csv))
      }
    if (statements.isEmpty) "" else statements.mkString("\n", "\n", "")
  }
}
