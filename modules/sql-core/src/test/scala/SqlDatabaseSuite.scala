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

package grackle.sql.test

import scala.concurrent.duration._

import munit.CatsEffectSuite

/**
 * Common base for the per-backend database suite traits, raising munit-cats-effect's 30s
 * default timeout: database-backed tests can exceed it for reasons that have nothing to do with
 * the test itself, e.g. several forked backend test JVMs contending for one docker daemon on a
 * constrained machine, showing up as spurious timeouts rather than failures.
 */
trait SqlDatabaseSuite extends CatsEffectSuite {
  override def munitIOTimeout: Duration = 2.minutes
}
