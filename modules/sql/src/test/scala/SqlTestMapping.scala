// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import org.tpolecat.sourcepos.SourcePos

import edu.gemini.grackle._
import sql.SqlMapping

trait SqlTestMapping[F[_]] extends SqlMapping[F] { outer =>
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
  def zonedDateTime: Codec
  def duration: Codec

  def jsonb: Codec

  def nullable(c: Codec): Codec
  def list(c: Codec): Codec

  def col[T](colName: String, codec: Codec)(implicit tableName: TableName, pos: SourcePos): ColumnRef =
    ColumnRef(tableName.name, colName, codec, null, pos)
}
