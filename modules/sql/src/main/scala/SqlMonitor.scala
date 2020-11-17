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
