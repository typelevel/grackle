// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package skunk

import _root_.skunk.AppliedFragment
import cats.Applicative
import cats.implicits._
import edu.gemini.grackle.QueryInterpreter.ProtoJson
import cats.data.StateT

trait SkunkMonitor[F[_]] {
  def stageStarted: F[Unit]
  def queryMapped(query: Query, fragment: AppliedFragment, table: List[Row]): F[Unit]
  def resultComputed(result: Result[ProtoJson]): F[Unit]
  def stageCompleted: F[Unit]
}

case class SkunkStats(
  query: Query,
  sql: String,
  args: Any,
  rows: Int,
  cols: Int
)

object SkunkMonitor {
  def noopMonitor[F[_]: Applicative]: SkunkMonitor[F] =
    new SkunkMonitor[F] {
      def stageStarted: F[Unit] = ().pure[F]
      def queryMapped(query: Query, fragment: AppliedFragment, table: List[Row]): F[Unit] = ().pure[F]
      def resultComputed(result: Result[ProtoJson]): F[Unit] = ().pure[F]
      def stageCompleted: F[Unit] = ().pure[F]
    }

//   def loggerMonitor[F[_]: Applicative](logger: Logger[F]): SkunkMonitor[F] =
//     new SkunkMonitor[F] {
//       import FragmentAccess._

//       def stageStarted: F[Unit] =
//         logger.info(s"stage started")

//       def queryMapped(query: Query, fragment: AppliedFragment, table: List[Row]): F[Unit] =
//         logger.info(
//           s"""query: $query
//              |sql: ${fragment.sql}
//              |args: ${fragment.args.mkString(", ")}
//              |fetched ${table.size} row(s) of ${table.headOption.map(_.elems.size).getOrElse(0)} column(s)
//            """.stripMargin)

//       def resultComputed(result: Result[ProtoJson]): F[Unit] =
//         logger.info(s"result: $result")

//       def stageCompleted: F[Unit] =
//         logger.info(s"stage completed")
//     }

  def stateMonitor[F[_]: Applicative]: SkunkMonitor[StateT[F, List[List[SkunkStats]], ?]] =
    new SkunkMonitor[StateT[F, List[List[SkunkStats]], ?]] {

      def stageStarted: StateT[F, List[List[SkunkStats]], Unit] =
        StateT.modify(states => Nil :: states)

      def queryMapped(query: Query, fragment: AppliedFragment, table: List[Row]): StateT[F, List[List[SkunkStats]], Unit] = {
        val stats =
          SkunkStats(
            query,
            fragment.fragment.sql,
            fragment.argument,
            table.size,
            table.headOption.map(_.elems.size).getOrElse(0),
          )

        StateT.modify {
          case Nil => List(List(stats))
          case hd :: tl => (stats :: hd) :: tl
        }
      }

      def resultComputed(result: Result[ProtoJson]): StateT[F, List[List[SkunkStats]], Unit] =
        StateT.pure(())

      def stageCompleted: StateT[F, List[List[SkunkStats]], Unit] =
        StateT.pure(())
    }
}
