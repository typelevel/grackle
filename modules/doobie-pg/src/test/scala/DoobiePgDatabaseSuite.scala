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

package grackle.doobie.postgres
package test

import cats.effect.{IO, Resource, Sync}
import doobie.Transactor
import munit.catseffect.IOFixture

import grackle.doobie.DoobieMonitor
import grackle.doobie.test.DoobieDatabaseSuite
import grackle.sql.test._
import grackle.sqlpg.test._

trait DoobiePgDatabaseSuite extends DoobieDatabaseSuite with SqlPgDatabaseSuite {
  abstract class DoobiePgTestMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F] = DoobieMonitor.noopMonitor[IO])
    extends DoobiePgMapping[F](transactor, monitor) with DoobieTestMapping[F] with SqlTestMapping[F]

  def transactorResource: Resource[IO, Transactor[IO]] = {
    val connInfo = postgresConnectionInfo
    import connInfo._

    Resource.pure(
      Transactor.fromDriverManager[IO](
        driverClassName,
        jdbcUrl,
        username,
        password,
        None
      )
    )
  }

  val transactorFixture: IOFixture[Transactor[IO]] = ResourceSuiteLocalFixture("doobiepg", transactorResource)
  override def munitFixtures: Seq[IOFixture[_]] = Seq(transactorFixture)

  def transactor: Transactor[IO] = transactorFixture()
}
