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

package grackle.doobie.oracle
package test

import java.sql.Timestamp
import java.time.{LocalTime, OffsetDateTime, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.util.Try

import cats.data.NonEmptyList
import cats.effect.{Resource, Sync, IO}
import cats.syntax.all._
import doobie.{Meta, Transactor}
import doobie.enumerated.JdbcType
import doobie.util.Put
import io.circe.Json
import io.circe.parser.parse
import munit.catseffect._

import grackle.doobie.DoobieMonitor
import grackle.doobie.test.DoobieDatabaseSuite

import grackle.sql.test._

trait DoobieOracleDatabaseSuite extends DoobieDatabaseSuite {
  abstract class DoobieOracleTestMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F] = DoobieMonitor.noopMonitor[IO])
    extends DoobieOracleMapping[F](transactor, monitor) with DoobieTestMapping[F] with SqlTestMapping[F] {
      override val uuid: TestCodec[UUID] = (
        Meta[String].tiemap(s => Try(UUID.fromString(s)).toEither.leftMap(_.getMessage))(_.toString),
        false
      )

      val localTimeFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("0 H:m:s.S")
      override val localTime: TestCodec[LocalTime] = (
        Meta[String].tiemap(s => Try(LocalTime.parse(s, localTimeFormat)).toEither.leftMap(_.getMessage))(_.format(localTimeFormat)),
        false
      )

      // Forget precise time zone for compatibility with Postgres. Nb. this is specific to this test suite.
      override val offsetDateTime: TestCodec[OffsetDateTime] = (
        Meta[Timestamp].timap(t => OffsetDateTime.ofInstant(t.toInstant, ZoneId.of("UTC")))(o => Timestamp.from(o.toInstant)),
        false
      )

      val nvarcharMeta: Meta[String] = {
        import JdbcType._
        val oldGet = Meta[String].get
        val oldPut = Meta[String].put
        val newTargets = NonEmptyList.of(NChar, NVarChar, LongnVarChar)
        val newPut =
          Put.Basic(oldPut.typeStack, newTargets, oldPut.put, oldPut.update, oldPut.vendorTypeNames.headOption)

        new Meta[String](oldGet, newPut)
      }

      override val nvarchar: TestCodec[String] = (
        nvarcharMeta,
        false
      )

      override val jsonb: TestCodec[Json] = (
        Meta[String].tiemap(s => parse(s).leftMap(_.getMessage))(_.noSpaces),
        false
      )
    }

  case class OracleConnectionInfo(host: String, port: Int) {
    val driverClassName = "oracle.jdbc.driver.OracleDriver"
    val databaseName = "FREEPDB1"
    val jdbcUrl = s"jdbc:oracle:thin:@//$host:$port/$databaseName"
    val username = "test"
    val password = "test"
  }
  object OracleConnectionInfo {
    val DefaultPort = 1521
  }

  val oracleConnectionInfo: OracleConnectionInfo = OracleConnectionInfo("localhost", OracleConnectionInfo.DefaultPort)

  def transactorResource: Resource[IO, Transactor[IO]] = {
    val connInfo = oracleConnectionInfo
    import connInfo._

    val props = new java.util.Properties()
    props.setProperty("user", username)
    props.setProperty("password", password)
    props.setProperty("oracle.jdbc.jsonDefaultGetObjectType", "java.lang.String")
    Resource.pure(
      Transactor.fromDriverManager[IO](
        driverClassName,
        jdbcUrl,
        props,
        None
      )
    )
  }

  val transactorFixture: IOFixture[Transactor[IO]] = ResourceSuiteLocalFixture("oraclepg", transactorResource)
  override def munitFixtures: Seq[IOFixture[_]] = Seq(transactorFixture)

  def transactor: Transactor[IO] = transactorFixture()
}
