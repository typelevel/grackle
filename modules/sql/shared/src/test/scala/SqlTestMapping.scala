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

import org.tpolecat.sourcepos.SourcePos

import grackle._
import sql.SqlMappingLike

trait SqlTestMapping[F[_]] extends SqlMappingLike[F] { outer =>
  def bool: Codec
  def text: Codec
  def varchar: Codec
  def bpchar(len: Int): Codec
  def int2: Codec
  def int4: Codec
  def int8: Codec
  def float4: Codec
  def float8: Codec
  def numeric(precision: Int, scale: Int): Codec

  def uuid: Codec
  def localDate: Codec
  def localTime: Codec
  def offsetDateTime: Codec
  def duration: Codec

  def jsonb: Codec

  def nullable(c: Codec): Codec
  def list(c: Codec): Codec

  def col[T](colName: String, codec: Codec)(implicit tableName: TableName, pos: SourcePos): ColumnRef =
    ColumnRef(tableName.name, colName, codec, null, pos)
}
