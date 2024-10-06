// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
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

package grackle.sqlpg
package test

import munit.CatsEffectSuite

trait SqlPgDatabaseSuite extends CatsEffectSuite {
  case class PostgresConnectionInfo(host: String, port: Int) {

    val driverClassName = "org.postgresql.Driver"
    val databaseName = "test"
    val jdbcUrl = s"jdbc:postgresql://$host:$port/$databaseName"
    val username = "test"
    val password = "test"
  }
  object PostgresConnectionInfo {
    val DefaultPort = 5432
  }

  val postgresConnectionInfo: PostgresConnectionInfo =
    PostgresConnectionInfo("localhost", PostgresConnectionInfo.DefaultPort)
}
