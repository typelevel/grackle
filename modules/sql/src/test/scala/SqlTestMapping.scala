// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package utils

import edu.gemini.grackle._
import sql.SqlMapping

trait SqlTestMapping[F[_]] extends SqlMapping[F] { outer =>
  def boolCol(table: String, name: String, nullable: Boolean): ColumnRef
  def textCol(table: String, name: String, nullable: Boolean): ColumnRef
  def varcharCol(table: String, name: String, nullable: Boolean): ColumnRef
  def bpcharCol(table: String, name: String, nullable: Boolean, len: Int): ColumnRef
  def int2Col(table: String, name: String, nullable: Boolean): ColumnRef
  def int4Col(table: String, name: String, nullable: Boolean): ColumnRef
  def int8Col(table: String, name: String, nullable: Boolean): ColumnRef
  def float4Col(table: String, name: String, nullable: Boolean): ColumnRef
  def float8Col(table: String, name: String, nullable: Boolean): ColumnRef
  def numericCol(table: String, name: String, nullable: Boolean, precision: Int, scale: Int): ColumnRef

  class TableDef(table: String) {
    def boolCol(name: String, nullable: Boolean): ColumnRef = outer.boolCol(table, name, nullable)
    def textCol(name: String, nullable: Boolean): ColumnRef = outer.textCol(table, name, nullable)
    def varcharCol(name: String, nullable: Boolean): ColumnRef = outer.varcharCol(table, name, nullable)
    def bpcharCol(name: String, nullable: Boolean, len: Int): ColumnRef = outer.bpcharCol(table, name, nullable, len)
    def int2Col(name: String, nullable: Boolean): ColumnRef = outer.int2Col(table, name, nullable)
    def int4Col(name: String, nullable: Boolean): ColumnRef = outer.int4Col(table, name, nullable)
    def int8Col(name: String, nullable: Boolean): ColumnRef = outer.int8Col(table, name, nullable)
    def float4Col(name: String, nullable: Boolean): ColumnRef = outer.float4Col(table, name, nullable)
    def float8Col(name: String, nullable: Boolean): ColumnRef = outer.float8Col(table, name, nullable)
    def numericCol(name: String, nullable: Boolean, precision: Int, scale: Int): ColumnRef = outer.numericCol(table, name, nullable, precision, scale)
  }
}
