// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package utils

import cats.effect.{Resource, Sync}
import skunk.Session

import edu.gemini.grackle._, skunk._

abstract class SkunkTestMapping[F[_]: Sync](pool: Resource[F,Session[F]], monitor: SkunkMonitor[F])
  extends SkunkMapping[F](pool, monitor) with SqlTestMapping[F] {
  import _root_.skunk.codec.all._

  def mkCodec[T](c: _root_.skunk.Codec[T], nullable: Boolean): Codec =
    (if (nullable) c.opt else c, nullable)

  def boolCol(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, mkCodec(bool, nullable), null, null)
  def textCol(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, mkCodec(text, nullable), null, null)
  def varcharCol(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, mkCodec(varchar, nullable), null, null)
  def bpcharCol(table: String, name: String, nullable: Boolean, len: Int): ColumnRef =
    ColumnRef(table, name, mkCodec(bpchar(len), nullable), null, null)
  def int2Col(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, mkCodec(int2.imap(_.toInt)(_.toShort), nullable), null, null)
  def int4Col(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, mkCodec(int4, nullable), null, null)
  def int8Col(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, mkCodec(int8, nullable), null, null)
  def float4Col(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, mkCodec(float4, nullable), null, null)
  def float8Col(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, mkCodec(float8, nullable), null, null)
  def numericCol(table: String, name: String, nullable: Boolean, precision: Int, scale: Int): ColumnRef =
    ColumnRef(table, name, mkCodec(numeric(precision, scale), nullable), null, null)
}
