// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package sql

import edu.gemini.grackle.QueryInterpreter.ProtoJson

/** Monitor for a `SqlMapping` in `F` with fragments of type `A`. */
trait SqlMonitor[F[_], A] {
  def stageStarted: F[Unit]
  def queryMapped(query: Query, fragment: A, table: List[Row]): F[Unit]
  def resultComputed(result: Result[ProtoJson]): F[Unit]
  def stageCompleted: F[Unit]
}
