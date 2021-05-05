// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql

/** A database row is a list of untyped values (some of which may be `FailedJoin`). */
case class Row(elems: List[Any]) {
  def apply(i: Int): Any = elems(i)
}
