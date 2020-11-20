// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package doobie

import cats.Reducible
import edu.gemini.grackle.sql._
import cats.effect.Sync
import edu.gemini.grackle.sql.NullabilityKnown
import _root_.doobie.{ Meta, Put, Fragment => DoobieFragment, Read, Transactor }
import _root_.doobie.enum.{ Nullability => DoobieNullability }
import _root_.doobie.enum.Nullability.{ NullabilityKnown => DoobieNullabilityKnown }
import _root_.doobie.implicits._
import _root_.doobie.util.fragments
import java.sql.ResultSet

abstract class DoobieMapping[F[_]: Sync](
      transactor: Transactor[F],
  val monitor:    DoobieMonitor[F]
) extends SqlMapping[F] {

  type Codec[A]   = Meta[A]
  type Encoder[A] = Put[A]
  type Fragment   = DoobieFragment

  def toEncoder[A](c: Meta[A]): Put[A] =
    c.put

  def intEncoder     = Put[Int]
  def stringEncoder  = Put[String]
  def booleanEncoder = Put[Boolean]
  def doubleEncoder  = Put[Double]

  implicit def Fragments: SqlFragment[Fragment] =
    new SqlFragment[Fragment] {
      def combine(x: Fragment, y: Fragment): Fragment = x ++ y
      def empty: Fragment = DoobieFragment.empty
      def bind[A](encoder: Put[A], nullable: Boolean, value: A): Fragment = { implicit val e = encoder ; sql"$value" }
      def const(s: String): Fragment = DoobieFragment.const(s)
      def andOpt(fs: Option[Fragment]*): Fragment = fragments.andOpt(fs: _*)
      def orOpt(fs: Option[Fragment]*): Fragment = fragments.orOpt(fs: _*)
      def whereAndOpt(fs: Option[Fragment]*): Fragment = fragments.whereAndOpt(fs: _*)
      def in[G[_]: Reducible, A](f: Fragment, fs: G[A], enc: Put[A]): Fragment = fragments.in(f, fs)(implicitly, enc)
    }

  def toDoobie(nn: NullabilityKnown): DoobieNullabilityKnown =
    nn match {
      case NoNulls  => DoobieNullability.NoNulls
      case Nullable => DoobieNullability.Nullable
    }

  def fetch(fragment: Fragment, metas: List[(Boolean, (Codec[_], NullabilityKnown))]): F[edu.gemini.grackle.sql.Table] = {

    def mkRead(metas: List[(Boolean, (Meta[_], NullabilityKnown))]): Read[Row] = {
      def unsafeGet(rs: ResultSet, n: Int): Row =
        Row {
          metas.zipWithIndex.map {
            case ((isJoin, (m, NoNulls)),  i) =>
              if (isJoin) m.get.unsafeGetNullable(rs, n+i).getOrElse(FailedJoin)
              else m.get.unsafeGetNonNullable(rs, n+i)
            case ((_, (m, Nullable)), i) => m.get.unsafeGetNullable(rs, n+i)
          }
        }
      new Read(metas.map { case (_, (m, n)) => (m.get, toDoobie(n)) }, unsafeGet)
    }

    fragment.query[Row](mkRead(metas)).to[List].transact(transactor)

  }

}

