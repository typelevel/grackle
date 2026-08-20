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

package grackle.doobie.sqlite.test

import java.io.File
import java.nio.file.{Files, Path}
import java.sql.DriverManager
import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter
import java.util.UUID

import scala.util.{Try, Using}

import cats.effect.{IO, Resource, Sync}
import cats.syntax.all._
import io.circe.{Decoder => CDecoder, Encoder => CEncoder, Json}
import io.circe.parser.parse
import io.circe.syntax._
import munit.catseffect._
import org.sqlite.SQLiteConfig
import org.typelevel.doobie.{Meta, Transactor}

import grackle.doobie.DoobieMonitor
import grackle.doobie.sqlite.DoobieSqliteMapping
import grackle.doobie.test.DoobieDatabaseSuite
import grackle.sql.test._

trait DoobieSqliteDatabaseSuite extends DoobieDatabaseSuite {
  abstract class DoobieSqliteTestMapping[F[_]: Sync](
      transactor: Transactor[F],
      monitor: DoobieMonitor[F] = DoobieMonitor.noopMonitor[IO])
      extends DoobieSqliteMapping[F](transactor, monitor)
      with DoobieTestMapping[F]
      with SqlTestMapping[F] {
    def mkTestCodec[T](meta: Meta[T]): TestCodec[T] = (meta, false)

    val uuid: TestCodec[UUID] =
      mkTestCodec(Meta[String].tiemap(s =>
        Try(UUID.fromString(s)).toEither.leftMap(_.getMessage))(_.toString))

    // SQLite has no native date/time types - store as ISO-8601 TEXT, the dialect's own convention.
    val localTime: TestCodec[LocalTime] =
      mkTestCodec(Meta[String].tiemap(s =>
        Try(LocalTime.parse(s)).toEither.leftMap(_.getMessage))(_.toString))

    val localDate: TestCodec[LocalDate] =
      mkTestCodec(Meta[String].tiemap(s =>
        Try(LocalDate.parse(s)).toEither.leftMap(_.getMessage))(_.toString))

    // The shared testdata/*/coalesce.sql, movies.sql etc. spell offsets as e.g.
    // '2020-05-27 21:00:00 +02:00' (space-separated, not the 'T'-separated ISO_OFFSET_DATE_TIME
    // OffsetDateTime.parse defaults to), matching the literal format used by every other backend's
    // testdata - so a custom formatter is used here rather than reformatting the shared data.
    val offsetDateTimeFormat: DateTimeFormatter =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss xxx")
    // Normalize to UTC on decode: SQLite has no native timestamptz, so the literal offset written
    // in the seed data (e.g. +02:00) is preserved verbatim in storage, unlike Postgres/Oracle/MSSQL
    // whose drivers hand back a UTC-normalized OffsetDateTime regardless of how the value was
    // stored. Without this, otherwise-correct results fail equality checks against the shared
    // expected-JSON fixtures, which are all written in Postgres's UTC ("Z") form.
    val offsetDateTime: TestCodec[OffsetDateTime] =
      mkTestCodec(
        Meta[String].tiemap(s =>
          Try(
            OffsetDateTime.parse(s, offsetDateTimeFormat).withOffsetSameInstant(ZoneOffset.UTC))
            .toEither
            .leftMap(_.getMessage))(_.format(offsetDateTimeFormat)))

    val nvarchar: TestCodec[String] = mkTestCodec(Meta[String])

    val jsonb: TestCodec[Json] =
      mkTestCodec(Meta[String].tiemap(s => parse(s).leftMap(_.getMessage))(_.noSpaces))

    // SQLite has no array type either - JSON-encode into TEXT, as MSSQL's test mapping does.
    override def list[T: CDecoder: CEncoder](c: TestCodec[T]): TestCodec[List[T]] = {
      def put(ts: List[T]): String = ts.asJson.noSpaces
      def get(s: String): Either[String, List[T]] =
        parse(s).map(_.as[List[T]].toOption.get).leftMap(_.getMessage)

      mkTestCodec(Meta[String].tiemap(get)(put))
    }
  }

  // Where the seed scripts live - see the `Test / javaOptions` setting for grackle-doobie-sqlite
  // in build.sbt, which points this at testdata/sqlite/ regardless of the fork's working directory.
  def testdataDir: File =
    new File(
      sys
        .props
        .getOrElse(
          "grackle.sqlite.testdata",
          throw new IllegalStateException(
            "grackle.sqlite.testdata system property not set; see build.sbt's doobiesqlite project")))

  // A fresh on-disk SQLite database, seeded from every script in testdataDir, torn down on
  // release. Unlike the container-backed backends there's no shared server to point at, so each
  // suite gets its own fully isolated copy of the schema.
  def transactorResource: Resource[IO, Transactor[IO]] = {
    def newDbFile: IO[Path] = IO.blocking(Files.createTempFile("grackle-sqlite-", ".db"))

    def deleteDbFile(path: Path): IO[Unit] =
      IO.blocking {
        val base = path.toString
        List(base, s"$base-journal", s"$base-wal", s"$base-shm").foreach(new File(_).delete())
      }.void

    def seedScript: IO[String] =
      IO.blocking {
        Option(testdataDir.listFiles((_, name) => name.endsWith(".sql")))
          .fold(List.empty[File])(_.toList)
          .sortBy(_.getName)
          .map(f => new String(Files.readAllBytes(f.toPath), "UTF-8"))
          .mkString("\n")
      }

    def jdbcUrl(path: Path): String = s"jdbc:sqlite:${path.toAbsolutePath}"

    def sqliteProperties: java.util.Properties = {
      val config = new SQLiteConfig()
      // Case-sensitive LIKE is a connection-level setting in SQLite (no per-expression
      // equivalent); DoobieSqliteMappingLike.likeToFragment relies on it being enabled to
      // distinguish the `caseInsensitive` predicate flag.
      config.enableCaseSensitiveLike(true)
      config.toProperties
    }

    // Seeded via a single native multi-statement exec over a throwaway plain-JDBC connection,
    // rather than through Doobie: sqlite-jdbc's JNI layer can't reliably survive ~150+ individual
    // PreparedStatement create/execute/close cycles against one connection on recent JDKs (that
    // many round trips through Doobie's `.update.run`, all sharing a connection, corrupts a native
    // statement handle and throws "prepared statement has been finalized" from Connection#close).
    // A single `Statement.executeUpdate` on the whole concatenated script sidesteps that entirely
    // and is also dramatically faster, since it's one native `sqlite3_exec` call instead of ~150.
    def seed(path: Path): IO[Unit] =
      for {
        script <- seedScript
        url = jdbcUrl(path)
        props = sqliteProperties
        _ <- IO.blocking {
          Using.resource(DriverManager.getConnection(url, props)) { conn =>
            Using.resource(conn.createStatement())(_.executeUpdate(script))
          }
        }
      } yield ()

    def mkTransactor(path: Path): Transactor[IO] =
      Transactor.fromDriverManager[IO](
        "org.sqlite.JDBC",
        jdbcUrl(path),
        sqliteProperties,
        None
      )

    val alloc =
      for {
        path <- newDbFile
        _ <- seed(path)
      } yield (path, mkTransactor(path))

    Resource.make(alloc)(t => deleteDbFile(t._1)).map(_._2)
  }

  val transactorFixture: IOFixture[Transactor[IO]] =
    ResourceSuiteLocalFixture("doobiesqlite", transactorResource)
  override def munitFixtures: Seq[IOFixture[_]] = Seq(transactorFixture)

  def transactor: Transactor[IO] = transactorFixture()
}
