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
 * Creates an empty dataset directory with a schema skeleton per dialect.
 */
object NewDataset {

  def apply(baseDir: File, names: Seq[String]): Unit = {
    require(names.nonEmpty, "usage: newDataset <name>")
    names.foreach(create(baseDir, _))
  }

  private def create(baseDir: File, name: String): Unit = {
    val dir = baseDir / "testdata" / name
    require(!dir.exists, s"testdata/$name already exists")
    IO.createDirectory(dir)
    skeletons(name).foreach {
      case (dialect, schema) =>
        val file = dir / s"$dialect.sql"
        IO.write(file, schema)
        println(s"created testdata/$name/${file.getName}")
    }
    println(s"add <table>.csv next to them for the rows, see testdata/README.md")
  }

  /**
   * One CREATE TABLE per dialect, spelled the way that dialect's other scripts spell it.
   */
  private def skeletons(name: String): List[(String, String)] = {
    val table = name.replace('-', '_')
    List(
      "pg" -> s"""|CREATE TABLE $table (
                  |    id TEXT PRIMARY KEY,
                  |    value TEXT NOT NULL
                  |);
                  |""".stripMargin,
      "oracle" -> s"""|CREATE TABLE $table (
                      |    id VARCHAR2(100) PRIMARY KEY,
                      |    value VARCHAR2(100) NOT NULL
                      |);
                      |""".stripMargin,
      "mssql" -> s"""|CREATE TABLE $table (
                     |    id VARCHAR(100) PRIMARY KEY,
                     |    value VARCHAR(100) NOT NULL
                     |);
                     |
                     |GO
                     |""".stripMargin
    )
  }
}
