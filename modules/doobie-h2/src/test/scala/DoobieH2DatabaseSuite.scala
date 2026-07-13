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

package grackle.doobie.h2.test

import java.io.File
import java.nio.file.Files
import java.sql.DriverManager
import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneOffset}
import java.util.UUID

import scala.util.Using

import cats.data.NonEmptyList
import cats.effect.{IO, Resource, Sync}
import cats.syntax.all._
import io.circe.{Decoder => CDecoder, Encoder => CEncoder, Json}
import io.circe.parser.parse
import munit.catseffect._
import org.typelevel.doobie.{Get, Meta, Put, Transactor}
import org.typelevel.doobie.enumerated.JdbcType
// H2's own implicits provide Meta instances for java.time types (JavaLocalTimeMeta etc., from
// H2JavaTimeMetaInstances); importing org.typelevel.doobie.implicits.javatimedrivernative._
// alongside this binds the same simple names via a second wildcard import, which makes them
// ambiguous by name and drops them from implicit scope entirely (a silent "not found" rather
// than an "ambiguous implicit" error) - so only the h2-specific import is kept.
import org.typelevel.doobie.h2.implicits._

import grackle.doobie.DoobieMonitor
import grackle.doobie.h2.DoobieH2Mapping
import grackle.doobie.test.DoobieDatabaseSuite
import grackle.sql.test._

trait DoobieH2DatabaseSuite extends DoobieDatabaseSuite {
  abstract class DoobieH2TestMapping[F[_]: Sync](
      transactor: Transactor[F],
      monitor: DoobieMonitor[F] = DoobieMonitor.noopMonitor[IO])
      extends DoobieH2Mapping[F](transactor, monitor)
      with DoobieTestMapping[F]
      with SqlTestMapping[F] {
    def mkTestCodec[T](meta: Meta[T]): TestCodec[T] = (meta, false)

    val uuid: TestCodec[UUID] = mkTestCodec(Meta[UUID])
    val localTime: TestCodec[LocalTime] = mkTestCodec(Meta[LocalTime])
    val localDate: TestCodec[LocalDate] = mkTestCodec(Meta[LocalDate])

    // H2 preserves whatever offset the stored literal carried (unlike Postgres, whose driver
    // hands back UTC-normalized values); the shared expected-JSON fixtures are written in
    // Postgres's UTC ("Z") form, so normalize on decode.
    val offsetDateTime: TestCodec[OffsetDateTime] =
      mkTestCodec(
        Meta[OffsetDateTime].timap(_.withOffsetSameInstant(ZoneOffset.UTC))(odt => odt))

    val nvarchar: TestCodec[String] = mkTestCodec(Meta[String])

    // H2's JSON type has no useful JDBC mapping - store JSON text in VARCHAR, as MSSQL does.
    val jsonb: TestCodec[Json] =
      mkTestCodec(Meta[String].tiemap(s => parse(s).leftMap(_.getMessage))(_.noSpaces))

    // Native VARCHAR ARRAY columns, read via rs.getArray(n).getArray() and written via
    // connection.createArrayOf + ps.setArray (Put.Advanced.array - the write half of the same
    // constructor DoobieTestMapping's inherited default list codec uses for Postgres's
    // "_VARCHAR"), both confirmed working against H2 2.4.240 by a standalone JDBC probe.
    //
    // The read half is hand-rolled rather than reused from Get.Advanced.array/Meta.Advanced.array:
    // that helper does `rs.getArray(n).getArray().asInstanceOf[Array[A]]`, a whole-array cast that
    // relies on the driver handing back an array reified as the element type (works for Postgres).
    // H2 hands back a reified Object[] regardless of the declared element type - a whole-array
    // cast to Array[String] then fails with ClassCastException, confirmed by triggering it here
    // before switching to the element-wise cast below (`Object[]` -> map each element to String
    // individually, which is safe since every element *is* a String instance at runtime, only the
    // array's own reified component type is Object).
    //
    // NOT doobie-h2's own Meta[Array[String]] (org.typelevel.doobie.h2.implicits.
    // unliftedStringArrayType) either: verified via the same JDBC probe that it is broken from the
    // read side too. It's built on Meta.Advanced.other[Array[Object]], which reads via
    // rs.getObject(n, classOf[Array[Object]]) - H2 rejects that conversion outright
    // ("Data conversion error converting CHARACTER VARYING to JAVA_OBJECT"), even for a value it
    // just wrote itself.
    //
    // The vendor type name matters beyond Get/Put too: DoobieMapping's sqlTypeName renders it
    // verbatim into `CAST(NULL AS <name>)` for ascribed nulls, and H2 only accepts the full
    // "VARCHAR ARRAY" spelling there - bare "ARRAY" (H2's own Meta's vendor name) is a syntax
    // error ("expected 'data type'"), while "_VARCHAR" (the Postgres-flavoured default) is
    // meaningless to H2.
    private val arrayStringMeta: Meta[Array[String]] = {
      val vendorTypeNames = NonEmptyList.of("VARCHAR ARRAY")
      val get: Get[Array[String]] = Get
        .Advanced
        .one[Array[String]](
          JdbcType.Array,
          vendorTypeNames,
          (rs, n) => {
            val a = rs.getArray(n)
            if (a == null) null
            // A null array *element* passes through this cast silently as `null` rather than
            // being rejected - fine today since no fixture uses a nullable-element list column,
            // but worth revisiting if one is ever added.
            else a.getArray.asInstanceOf[Array[AnyRef]].map(_.asInstanceOf[String])
          }
        )
      val put: Put[Array[String]] = Put.Advanced.array[String](vendorTypeNames, "VARCHAR")
      new Meta(get, put)
    }

    override def list[T: CDecoder: CEncoder](c: TestCodec[T]): TestCodec[List[T]] = {
      val cm = c._1
      val decode = cm.get.get.k.asInstanceOf[String => T]
      val encode = cm.put.put.k.asInstanceOf[T => String]
      mkTestCodec(arrayStringMeta.imap(_.toList.map(decode))(_.map(encode).toArray))
    }
  }

  // Where the seed scripts live - see the `Test / javaOptions` setting for grackle-doobie-h2
  // in build.sbt, which points this at testdata/h2/ regardless of the fork's working directory.
  def testdataDir: File =
    new File(
      sys
        .props
        .getOrElse(
          "grackle.h2.testdata",
          throw new IllegalStateException(
            "grackle.h2.testdata system property not set; see build.sbt's doobieh2 project")))

  // A fresh named in-memory H2 database, seeded from every script in testdataDir. DB_CLOSE_DELAY
  // keeps it alive between connections (each doobie transaction opens a new one); the explicit
  // SHUTDOWN on release drops it so it doesn't outlive its suite.
  def transactorResource: Resource[IO, Transactor[IO]] = {
    val url = s"jdbc:h2:mem:grackle-${UUID.randomUUID()};DB_CLOSE_DELAY=-1"

    def seedScript: IO[String] =
      IO.blocking {
        Option(testdataDir.listFiles((_, name) => name.endsWith(".sql")))
          .fold(List.empty[File])(_.toList)
          .sortBy(_.getName)
          .map(f => new String(Files.readAllBytes(f.toPath), "UTF-8"))
          .mkString("\n")
      }

    def exec(sql: String): IO[Unit] =
      IO.blocking {
        Using.resource(DriverManager.getConnection(url, "sa", "")) { conn =>
          Using.resource(conn.createStatement())(_.execute(sql))
        }
      }.void

    val mkTransactor =
      Transactor.fromDriverManager[IO]("org.h2.Driver", url, "sa", "", None)

    Resource.make(seedScript.flatMap(exec).as(mkTransactor))(_ => exec("SHUTDOWN"))
  }

  val transactorFixture: IOFixture[Transactor[IO]] =
    ResourceSuiteLocalFixture("doobieh2", transactorResource)
  override def munitFixtures: Seq[IOFixture[_]] = Seq(transactorFixture)

  def transactor: Transactor[IO] = transactorFixture()
}
