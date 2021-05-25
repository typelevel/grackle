// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package doobie

import cats.Reducible
import edu.gemini.grackle.sql._
import cats.effect.Sync
import _root_.doobie.{ Meta, Put, Fragment => DoobieFragment, Read, Transactor }
import _root_.doobie.enumerated.Nullability.{Nullable, NoNulls}
import _root_.doobie.implicits._
import _root_.doobie.util.fragments
import java.sql.ResultSet
import scala.annotation.unchecked.uncheckedVariance

abstract class DoobieMapping[F[_]: Sync](
  val transactor: Transactor[F],
  val monitor:    DoobieMonitor[F]
) extends SqlMapping[F] {

  type Codec[A]   = (Meta[A], Boolean)
  type Encoder[-A] = (Put[A], Boolean) @uncheckedVariance
  type Fragment   = DoobieFragment

  def toEncoder[A](c: Codec[A]): Encoder[A] = (c._1.put, c._2)

  def intEncoder     = (Put[Int], false)
  def stringEncoder  = (Put[String], false)
  def booleanEncoder = (Put[Boolean], false)
  def doubleEncoder  = (Put[Double], false)

  class TableDef(name: String) {
    def col(colName: String, codec: Meta[_], nullable: Boolean = false): ColumnRef =
      ColumnRef(name, colName, (codec, nullable))
  }

  implicit def Fragments: SqlFragment[Fragment] =
    new SqlFragment[Fragment] {
      def combine(x: Fragment, y: Fragment): Fragment = x ++ y
      def empty: Fragment = DoobieFragment.empty
      def bind[A](encoder: Encoder[A], value: A): Fragment = {
        val (e0, nullable) = encoder
        implicit val e = e0.asInstanceOf[Put[A]]
        if (!nullable) sql"$value"
        else
          value.asInstanceOf[Option[A]] match {
            case Some(v) => sql"$v"
            case None => sql"NULL"
          }
      }
      def const(s: String): Fragment = DoobieFragment.const(s)
      def andOpt(fs: Option[Fragment]*): Fragment = fragments.andOpt(fs: _*)
      def orOpt(fs: Option[Fragment]*): Fragment = fragments.orOpt(fs: _*)
      def whereAndOpt(fs: Option[Fragment]*): Fragment = fragments.whereAndOpt(fs: _*)
      def in[G[_]: Reducible, A](f: Fragment, fs: G[A], enc: Encoder[A]): Fragment = {
        val (put, _) = enc
        fragments.in(f, fs)(implicitly, put.asInstanceOf[Put[A]])
      }
    }

  def fetch(fragment: Fragment, metas: List[(Boolean, ExistentialCodec)]): F[edu.gemini.grackle.sql.Table] = {

    def mkRead(metas: List[(Boolean, ExistentialCodec)]): Read[Row] = {

      def unsafeGet(rs: ResultSet, n: Int): Row =
        Row {
          metas.zipWithIndex.map { case ((b, ec), i) =>
            ((b, ec.codec), i) match {
              case ((isJoin, (m, false)),  i) =>
                if (isJoin) m.get.unsafeGetNullable(rs, n+i).getOrElse(FailedJoin)
                else m.get.unsafeGetNonNullable(rs, n+i)
              case ((_, (m, true)), i) => m.get.unsafeGetNullable(rs, n+i)
            }
          }
        }
      new Read(metas.map { case (_, ec) =>
        val (m, n) = ec.codec
        (m.get, if(n) Nullable else NoNulls)
      }, unsafeGet)
    }

    fragment.query[Row](mkRead(metas)).to[List].transact(transactor)
  }
}
