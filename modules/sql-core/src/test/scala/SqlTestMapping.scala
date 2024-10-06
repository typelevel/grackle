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

package grackle.sql.test

import java.time.{Duration, LocalDate, LocalTime, OffsetDateTime}
import java.util.UUID

import io.circe.Json
import org.tpolecat.sourcepos.SourcePos
import org.tpolecat.typename.TypeName

import grackle._
import sql.SqlMappingLike

trait SqlTestMapping[F[_]] extends SqlMappingLike[F] { outer =>
  type TestCodec[T] <: Codec

  def bool: TestCodec[Boolean]
  def text: TestCodec[String]
  def varchar: TestCodec[String]
  def nvarchar: TestCodec[String]
  def bpchar(len: Int): TestCodec[String]
  def int2: TestCodec[Int]
  def int4: TestCodec[Int]
  def int8: TestCodec[Long]
  def float4: TestCodec[Float]
  def float8: TestCodec[Double]
  def numeric(precision: Int, scale: Int): TestCodec[BigDecimal]

  def uuid: TestCodec[UUID]
  def localDate: TestCodec[LocalDate]
  def localTime: TestCodec[LocalTime]
  def offsetDateTime: TestCodec[OffsetDateTime]
  def duration: TestCodec[Duration]

  def jsonb: TestCodec[Json]

  def nullable[T](c: TestCodec[T]): TestCodec[T]
  def list[T](c: TestCodec[T]): TestCodec[List[T]]

  def col[T](colName: String, codec: TestCodec[T])(implicit tableName: TableName, typeName: TypeName[T], pos: SourcePos): ColumnRef =
    ColumnRef(tableName.name, colName, codec, typeName.value, pos)
}
