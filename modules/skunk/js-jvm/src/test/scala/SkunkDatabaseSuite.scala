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

package grackle.skunk.test

import java.time.{Duration, LocalDate, LocalTime, OffsetDateTime}
import java.util.UUID

import cats.effect.{IO, Resource, Sync}
import io.circe.{Decoder => CDecoder, Encoder => CEncoder, Json}
import munit.catseffect.IOFixture
import natchez.Trace.Implicits.noop
import skunk.{ Codec => SCodec, Session }
import skunk.codec.{ all => codec }
import skunk.circe.codec.{ all => ccodec }

import grackle._, skunk._

import grackle.sql.test._
import grackle.sqlpg.test._

trait SkunkDatabaseSuite extends SqlPgDatabaseSuite {

  def sessionResource: Resource[IO, Session[IO]] = {
    val connInfo = postgresConnectionInfo
    import connInfo._

    Session.single[IO](
      host     = host,
      port     = port,
      user     = username,
      password = Some(password),
      database = databaseName,
      //debug    = true,
    )
  }

  val sessionFixture: IOFixture[Session[IO]] = ResourceSuiteLocalFixture("skunk", sessionResource)
  override def munitFixtures: Seq[IOFixture[_]] = Seq(sessionFixture)

  def session: Session[IO] = sessionFixture()

  abstract class SkunkTestMapping[F[_]: Sync](session: Session[F], monitor: SkunkMonitor[F] = SkunkMonitor.noopMonitor[IO])
    extends SkunkMapping[F](session, monitor) with SqlTestMapping[F] {

    type TestCodec[T] = (SCodec[T], Boolean)

    def bool: TestCodec[Boolean] = (codec.bool, false)
    def text: TestCodec[String] = (codec.text, false)
    def varchar: TestCodec[String] = (codec.varchar, false)
    def nvarchar: TestCodec[String] = (codec.text, false) // For compatbiltity with Oracle in these Suites.
    def bpchar(len: Int): TestCodec[String] = (codec.bpchar(len), false)
    def int2: TestCodec[Int] = (codec.int2.imap(_.toInt)(_.toShort), false)
    def int4: TestCodec[Int] = (codec.int4, false)
    def int8: TestCodec[Long] = (codec.int8, false)
    def float4: TestCodec[Float] = (codec.float4, false)
    def float8: TestCodec[Double] = (codec.float8, false)
    def numeric(precision: Int, scale: Int): TestCodec[BigDecimal] = (codec.numeric(precision, scale), false)

    def uuid: TestCodec[UUID] = (codec.uuid, false)
    def localDate: TestCodec[LocalDate] = (codec.date, false)
    def localTime: TestCodec[LocalTime] = (codec.time, false)
    def offsetDateTime: TestCodec[OffsetDateTime] = (codec.timestamptz, false)
    def duration: TestCodec[Duration] = (codec.int8.imap(Duration.ofMillis)(_.toMillis), false)

    def jsonb: TestCodec[Json] = (ccodec.jsonb, false)

    def nullable[T](c: TestCodec[T]): TestCodec[T] = (c._1.opt, true).asInstanceOf[TestCodec[T]]

    def list[T: CDecoder : CEncoder](c: TestCodec[T]): TestCodec[List[T]] = {
      val cc = c._1.asInstanceOf[SCodec[Any]]
      val ty = _root_.skunk.data.Type(s"_${cc.types.head.name}", cc.types)
      val encode = (elem: Any) => cc.encode(elem).head.get
      val decode = (str: String) => cc.decode(0, List(Some(str))).left.map(_.message)
      (SCodec.array(encode, decode, ty), false).asInstanceOf[TestCodec[List[T]]]
    }
  }
}
