// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.skunk.test

import java.time.Duration

import cats.effect.{IO, Resource, Sync}
import natchez.Trace.Implicits.noop
import org.testcontainers.containers.PostgreSQLContainer.POSTGRESQL_PORT
import skunk.Session
import skunk.codec.{ all => codec }
import skunk.circe.codec.{ all => ccodec }

import edu.gemini.grackle._, skunk._

import edu.gemini.grackle.sql.test._

trait SkunkDatabaseSuite extends SqlDatabaseSuite {

  // Slow because each usage will open a new socket, but ok for now.
  lazy val pool: Resource[IO, Session[IO]] =
    Session.single[IO](
      host     = container.containerIpAddress,
      port     = container.mappedPort(POSTGRESQL_PORT),
      user     = container.username,
      password = Some(container.password),
      database = container.databaseName,
      // debug    = true,
    )

  abstract class SkunkTestMapping[F[_]: Sync](pool: Resource[F,Session[F]], monitor: SkunkMonitor[F] = SkunkMonitor.noopMonitor[IO])
    extends SkunkMapping[F](pool, monitor) with SqlTestMapping[F] {
    def bool: Codec = (codec.bool, false)
    def text: Codec = (codec.text, false)
    def varchar: Codec = (codec.varchar, false)
    def bpchar(len: Int): Codec = (codec.bpchar(len), false)
    def int2: Codec = (codec.int2.imap(_.toInt)(_.toShort), false)
    def int4: Codec = (codec.int4, false)
    def int8: Codec = (codec.int8, false)
    def float4: Codec = (codec.float4, false)
    def float8: Codec = (codec.float8, false)
    def numeric(precision: Int, scale: Int): Codec = (codec.numeric(precision, scale), false)

    def uuid: Codec = (codec.uuid, false)
    def localDate: Codec = (codec.date, false)
    def localTime: Codec = (codec.time, false)
    def zonedDateTime: Codec = (codec.timestamptz.imap(_.toZonedDateTime)(_.toOffsetDateTime), false)
    def duration: Codec = (codec.int8.imap(Duration.ofMillis)(_.toMillis), false)

    def jsonb: Codec = (ccodec.jsonb, false)

    def nullable(c: Codec): Codec = (c._1.opt, true)

    def list(c: Codec): Codec = {
      val cc = c._1.asInstanceOf[_root_.skunk.Codec[Any]]
      val ty = _root_.skunk.data.Type(s"_${cc.types.head.name}", cc.types) 
      val encode = (elem: Any) => cc.encode(elem).head.get
      val decode = (str: String) => cc.decode(0, List(Some(str))).left.map(_.message)
      (_root_.skunk.Codec.array(encode, decode, ty), false)
    }
  }
}
