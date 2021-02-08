// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package doobie

import _root_.doobie.Fragment
import cats.Applicative
import cats.data.StateT
import cats.implicits._
import edu.gemini.grackle.QueryInterpreter.ProtoJson
import edu.gemini.grackle.sql.Row
import io.chrisdavenport.log4cats.Logger

trait DoobieMonitor[F[_]] extends edu.gemini.grackle.sql.SqlMonitor[F, Fragment] {
  def stageStarted: F[Unit]
  def queryMapped(query: Query, fragment: Fragment, table: List[Row]): F[Unit]
  def resultComputed(result: Result[ProtoJson]): F[Unit]
  def stageCompleted: F[Unit]
}

case class DoobieStats(
  query: Query,
  sql: String,
  args: List[Any],
  rows: Int,
  cols: Int
)

object DoobieMonitor {

  private implicit class FragmentOps(f: Fragment) {
    def sql: String = {
      val m = f.getClass.getDeclaredField("sql")
      m.setAccessible(true)
      m.get(f).asInstanceOf[String]
    }

    def args: List[Any] = {
      val m = f.getClass.getDeclaredMethod("args")
      m.setAccessible(true)
      m.invoke(f).asInstanceOf[List[Any]]
    }
  }

  def noopMonitor[F[_]: Applicative]: DoobieMonitor[F] =
    new DoobieMonitor[F] {
      def stageStarted: F[Unit] = ().pure[F]
      def queryMapped(query: Query, fragment: Fragment, table: List[Row]): F[Unit] = ().pure[F]
      def resultComputed(result: Result[ProtoJson]): F[Unit] = ().pure[F]
      def stageCompleted: F[Unit] = ().pure[F]
    }

  def loggerMonitor[F[_]: Applicative](logger: Logger[F]): DoobieMonitor[F] =
    new DoobieMonitor[F] {

      def stageStarted: F[Unit] =
        logger.info(s"stage started")

      def queryMapped(query: Query, fragment: Fragment, table: List[Row]): F[Unit] =
        logger.info(
          s"""query: $query
             |sql: ${fragment.sql}
             |args: ${fragment.args.mkString(", ")}
             |fetched ${table.size} row(s) of ${table.headOption.map(_.elems.size).getOrElse(0)} column(s)
           """.stripMargin)

      def resultComputed(result: Result[ProtoJson]): F[Unit] =
        logger.info(s"result: $result")

      def stageCompleted: F[Unit] =
        logger.info(s"stage completed")
    }

  def stateMonitor[F[_]: Applicative]: DoobieMonitor[StateT[F, List[List[DoobieStats]], *]] =
    new DoobieMonitor[StateT[F, List[List[DoobieStats]], *]] {

      def stageStarted: StateT[F, List[List[DoobieStats]], Unit] =
        StateT.modify(states => Nil :: states)

      def queryMapped(query: Query, fragment: Fragment, table: List[Row]): StateT[F, List[List[DoobieStats]], Unit] = {
        val stats =
          DoobieStats(
            query,
            fragment.sql,
            fragment.args,
            table.size,
            table.headOption.map(_.elems.size).getOrElse(0),
          )

        StateT.modify {
          case Nil => List(List(stats))
          case hd :: tl => (stats :: hd) :: tl
        }
      }

      def resultComputed(result: Result[ProtoJson]): StateT[F, List[List[DoobieStats]], Unit] =
        StateT.pure(())

      def stageCompleted: StateT[F, List[List[DoobieStats]], Unit] =
        StateT.pure(())
    }
}
