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

package grackle.doobie.test

import java.time.{Duration, LocalDate, LocalTime, OffsetDateTime}
import java.util.UUID

import cats.effect.{IO, Sync}
import doobie.postgres.implicits._
import doobie.postgres.circe.jsonb.implicits._
import doobie.{Get, Meta, Put, Transactor}
import io.circe.Json

import grackle.doobie.postgres.{DoobieMapping, DoobieMonitor}

import grackle.sql.test._

trait DoobieDatabaseSuite extends SqlDatabaseSuite {
  // lazy vals because the container is not initialised until the test is run
  lazy val xa = {
    val connInfo = postgresConnectionInfo()
    import connInfo._

    Transactor.fromDriverManager[IO](
      driverClassName,
      jdbcUrl,
      username,
      password,
      None
    )
  }

  abstract class DoobieTestMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F] = DoobieMonitor.noopMonitor[IO])
    extends DoobieMapping[F](transactor, monitor) with SqlTestMapping[F] {

    def bool: Codec = (Meta[Boolean], false)
    def text: Codec = (Meta[String], false)
    def varchar: Codec = (Meta[String], false)
    def bpchar(len: Int): Codec = (Meta[String], false)
    def int2: Codec = (Meta[Int], false)
    def int4: Codec = (Meta[Int], false)
    def int8: Codec = (Meta[Long], false)
    def float4: Codec = (Meta[Float], false)
    def float8: Codec = (Meta[Double], false)
    def numeric(precision: Int, scale: Int): Codec = (Meta[BigDecimal], false)

    def uuid: Codec = (Meta[UUID], false)
    def localDate: Codec = (Meta[LocalDate], false)
    def localTime: Codec = (Meta[LocalTime], false)
    def offsetDateTime: Codec = (Meta[OffsetDateTime], false)
    def duration: Codec = (Meta[Long].timap(Duration.ofMillis)(_.toMillis), false)

    def jsonb: Codec = (new Meta(Get[Json], Put[Json]), false)

    def nullable(c: Codec): Codec = (c._1, true)

    def list(c: Codec): Codec = {
      val cm = c._1.asInstanceOf[Meta[Any]]
      val decode = cm.get.get.k.asInstanceOf[String => Any]
      val encode = cm.put.put.k.asInstanceOf[Any => String]
      val cl: Meta[List[Any]] = Meta.Advanced.array[String]("VARCHAR", "_VARCHAR").imap(_.toList.map(decode))(_.map(encode).toArray)
      (cl, false)
    }
  }
}
