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

package grackle.doobie.mssql
package test

import java.sql.{Time, Timestamp}
import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneId}
import java.util.UUID
import scala.util.Try

import cats.effect.{Resource, Sync, IO}
import cats.syntax.all._
import doobie.{Meta, Transactor}
import doobie.enumerated.JdbcType
import doobie.util.meta.MetaConstructors.Basic
import io.circe.{Decoder => CDecoder, Encoder => CEncoder, Json}
import io.circe.syntax._
import io.circe.parser.parse
import munit.catseffect._

import grackle.doobie.DoobieMonitor
import grackle.doobie.test.DoobieDatabaseSuite

import grackle.sql.test._

trait DoobieMSSqlDatabaseSuite extends DoobieDatabaseSuite {
  abstract class DoobieMSSqlTestMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F] = DoobieMonitor.noopMonitor[IO])
    extends DoobieMSSqlMapping[F](transactor, monitor) with DoobieTestMapping[F] with SqlTestMapping[F] {
      def mkTestCodec[T](meta: Meta[T]): TestCodec[T] = (meta, false)

      val uuid: TestCodec[UUID] =
        mkTestCodec(Meta[String].tiemap(s => Try(UUID.fromString(s)).toEither.leftMap(_.getMessage))(_.toString))

      val localTime: TestCodec[LocalTime] = {
        mkTestCodec(Meta[Time].timap(t => LocalTime.ofNanoOfDay(t.toLocalTime.toNanoOfDay))(lt => Time.valueOf(lt)))
      }

      val localDate: TestCodec[LocalDate] =
        (Basic.oneObject(JdbcType.Date, None, classOf[LocalDate]), false)

      // Forget precise time zone for compatibility with Postgres. Nb. this is specific to this test suite.
      val offsetDateTime: TestCodec[OffsetDateTime] =
        mkTestCodec(Meta[Timestamp].timap(t => OffsetDateTime.ofInstant(t.toInstant, ZoneId.of("UTC")))(o => Timestamp.from(o.toInstant)))

      val nvarchar: TestCodec[String] = mkTestCodec(Meta[String])

      val jsonb: TestCodec[Json] =
        mkTestCodec(Meta[String].tiemap(s => parse(s).leftMap(_.getMessage))(_.noSpaces))

      override def list[T: CDecoder : CEncoder](c: TestCodec[T]): TestCodec[List[T]] = {
        def put(ts: List[T]): String = ts.asJson.noSpaces
        def get(s: String): Either[String, List[T]] = parse(s).map(_.as[List[T]].toOption.get).leftMap(_.getMessage)

        mkTestCodec(Meta[String].tiemap(get)(put))
      }
    }

  case class MSSqlConnectionInfo(host: String, port: Int) {
    val driverClassName = "com.microsoft.sqlserver.jdbc.SQLServerDriver"
    val databaseName = "test"
    val username = "sa"
    val password = "Test_123_Test"
    val jdbcUrl = s"jdbc:sqlserver://$host:$port;databaseName=$databaseName;user=$username;password=$password;trustServerCertificate=true;sendTimeAsDatetime=false;"
  }

  object MSSqlConnectionInfo {
    val DefaultPort = 1433
  }

  val msSqlConnectionInfo: MSSqlConnectionInfo =
    MSSqlConnectionInfo("localhost", MSSqlConnectionInfo.DefaultPort)

  def transactorResource: Resource[IO, Transactor[IO]] = {
    val connInfo = msSqlConnectionInfo
    import connInfo._

    val props = new java.util.Properties()
    Resource.pure(
      Transactor.fromDriverManager[IO](
        driverClassName,
        jdbcUrl,
        props,
        None
      )
    )
  }

  val transactorFixture: IOFixture[Transactor[IO]] = ResourceSuiteLocalFixture("mssqlpg", transactorResource)
  override def munitFixtures: Seq[IOFixture[_]] = Seq(transactorFixture)

  def transactor: Transactor[IO] = transactorFixture()
}
