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

package grackle.doobie
package test

import java.time.{Duration, LocalDate, LocalTime, OffsetDateTime}
import java.util.UUID

// TODO: Postgres-specifics will have to move
import doobie.postgres.implicits._
import doobie.postgres.circe.jsonb.implicits._
import doobie.{Get, Meta, Put}
import io.circe.Json
import munit.CatsEffectSuite

import grackle.sql.test._

trait DoobieDatabaseSuite extends CatsEffectSuite {
  trait DoobieTestMapping[F[_]] extends DoobieMappingLike[F] with SqlTestMapping[F] {

    type TestCodec[T] = (Meta[T], Boolean)

    def bool: TestCodec[Boolean] = (Meta[Boolean], false)
    def text: TestCodec[String] = (Meta[String], false)
    def varchar: TestCodec[String] = (Meta[String], false)
    def nvarchar: TestCodec[String] = (Meta[String], false)
    def bpchar(len: Int): TestCodec[String] = (Meta[String], false)
    def int2: TestCodec[Int] = (Meta[Int], false)
    def int4: TestCodec[Int] = (Meta[Int], false)
    def int8: TestCodec[Long] = (Meta[Long], false)
    def float4: TestCodec[Float] = (Meta[Float], false)
    def float8: TestCodec[Double] = (Meta[Double], false)
    def numeric(precision: Int, scale: Int): TestCodec[BigDecimal] = (Meta[BigDecimal], false)

    def uuid: TestCodec[UUID] = (Meta[UUID], false)
    def localDate: TestCodec[LocalDate] = (Meta[LocalDate], false)
    def localTime: TestCodec[LocalTime] = (Meta[LocalTime], false)
    def offsetDateTime: TestCodec[OffsetDateTime] = (Meta[OffsetDateTime], false)
    def duration: TestCodec[Duration] = (Meta[Long].timap(Duration.ofMillis)(_.toMillis), false)

    def jsonb: TestCodec[Json] = (new Meta(Get[Json], Put[Json]), false)

    def nullable[T](c: TestCodec[T]): TestCodec[T] = (c._1, true)

    def list[T](c: TestCodec[T]): TestCodec[List[T]] = {
      val cm = c._1
      val decode = cm.get.get.k.asInstanceOf[String => T]
      val encode = cm.put.put.k.asInstanceOf[T => String]
      val cl = Meta.Advanced.array[String]("VARCHAR", "_VARCHAR").imap(_.toList.map(decode))(_.map(encode).toArray)
      (cl, false)
    }
  }
}
