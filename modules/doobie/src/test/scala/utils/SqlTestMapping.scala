// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package utils

import cats.effect.Sync

import doobie.util.meta.Meta
import doobie.util.transactor.Transactor

import edu.gemini.grackle._
import doobie.{DoobieMapping, DoobieMonitor}

abstract class DoobieTestMapping[F[_]: Sync](transactor: Transactor[F], monitor: DoobieMonitor[F])
  extends DoobieMapping[F](transactor, monitor) with SqlTestMapping[F] {

  def boolCol(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, (Meta[Boolean], nullable), null, null)
  def textCol(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, (Meta[String], nullable), null, null)
  def varcharCol(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, (Meta[String], nullable), null, null)
  def bpcharCol(table: String, name: String, nullable: Boolean, len: Int): ColumnRef =
    ColumnRef(table, name, (Meta[String], nullable), null, null)
  def int2Col(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, (Meta[Int], nullable), null, null)
  def int4Col(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, (Meta[Int], nullable), null, null)
  def int8Col(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, (Meta[Long], nullable), null, null)
  def float4Col(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, (Meta[Float], nullable), null, null)
  def float8Col(table: String, name: String, nullable: Boolean): ColumnRef =
    ColumnRef(table, name, (Meta[Double], nullable), null, null)
  def numericCol(table: String, name: String, nullable: Boolean, precision: Int, scale: Int): ColumnRef =
    ColumnRef(table, name, (Meta[BigDecimal], nullable), null, null)
}
