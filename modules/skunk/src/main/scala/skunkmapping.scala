// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package skunk

import _root_.skunk.{ AppliedFragment, Decoder, Session }
import _root_.skunk.codec.all.{ bool, varchar, int4, float8 }
import _root_.skunk.implicits._
import cats.Reducible
import cats.effect.{ Resource, Sync }
import cats.implicits._
import edu.gemini.grackle.sql._
import scala.util.control.NonFatal

abstract class SkunkMapping[F[_]: Sync](
      pool:    Resource[F, Session[F]],
  val monitor: SkunkMonitor[F]
) extends SqlMapping[F] { outer =>

  // Grackle needs to know about codecs, encoders, and fragments.
  type Codec[A]   = _root_.skunk.Codec[A]
  type Encoder[A] = _root_.skunk.Encoder[A]
  type Fragment   = _root_.skunk.AppliedFragment

  def toEncoder[A](c: Codec[A]): Encoder[A] = c

  // Also we need to know how to encode the basic GraphQL types.
  def booleanEncoder = bool
  def stringEncoder  = varchar
  def doubleEncoder  = float8
  def intEncoder     = int4

  // We need to demonstrate that our `Fragment` type has certain compositional properties.
  implicit def Fragments: SqlFragment[AppliedFragment] =
    new SqlFragment[AppliedFragment] {
      def combine(x: AppliedFragment, y: AppliedFragment) = x |+| y
      def empty = AppliedFragment.empty
      def bind[A](encoder: Encoder[A], nullable: Boolean, value: A) =
        sql"$encoder".apply(if(!nullable) value else Some(value).asInstanceOf[A])
      def const(s: String) = sql"#$s".apply(_root_.skunk.Void)
      def in[G[_]: Reducible, A](f: AppliedFragment, fs: G[A], enc: Encoder[A]): AppliedFragment = fs.toList.map(sql"$enc".apply).foldSmash(f |+| void" IN (", void", ", void")")
      def and(fs: AppliedFragment*): AppliedFragment = fs.toList.map(parentheses).intercalate(void" AND ")
      def andOpt(fs: Option[AppliedFragment]*): AppliedFragment = and(fs.toList.unite: _*)
      def or(fs: AppliedFragment*): AppliedFragment = fs.toList.map(parentheses).intercalate(void" OR ")
      def orOpt(fs: Option[AppliedFragment]*): AppliedFragment = or(fs.toList.unite: _*)
      def whereAnd(fs: AppliedFragment*): AppliedFragment = if (fs.isEmpty) AppliedFragment.empty else void"WHERE " |+| and(fs: _*)
      def whereAndOpt(fs: Option[AppliedFragment]*): AppliedFragment = whereAnd(fs.toList.unite: _*)
      def parentheses(f: AppliedFragment): AppliedFragment = void"(" |+| f |+| void")"
    }

  // And we need to be able to fetch `Rows` given a `Fragment` and a list of decoders.
  def fetch(fragment: Fragment, metas: List[(Boolean, (Codec[_], NullabilityKnown))]): F[Table] = {

    lazy val rowDecoder: Decoder[Row] =
      new Decoder[Row] {

        lazy val types = metas.flatMap { case (_, (d, _)) => d.types }

        lazy val decodersWithOffsets: List[(Boolean, Decoder[_], NullabilityKnown, Int)] =
          metas.foldLeft((0, List.empty[(Boolean, Decoder[_], NullabilityKnown, Int)])) {
            case ((offset, accum), (isJoin, (decoder, nullity))) =>
              (offset + decoder.length, (isJoin, decoder, nullity, offset) :: accum)
          } ._2.reverse

        def decode(start: Int, ssx: List[Option[String]]): Either[Decoder.Error, Row] = {
          val ss = ssx.drop(start)
          decodersWithOffsets.traverse {

            // If the column is the outer part of a join and it's a non-nullable in the schema then
            // we read it as an option and collapse it, using FailedJoin in the None case. Otherwise
            // read as normal.
            case (true, decoder, NoNulls, offset) => decoder.opt.decode(0, ss.drop(offset).take(decoder.length)).map(_.getOrElse(FailedJoin))
            case (_,    decoder, _,       offset) => decoder    .decode(0, ss.drop(offset).take(decoder.length))

          } .map(Row(_))
        }

      }

    pool.use { s =>
      s.prepare(fragment.fragment.query(rowDecoder)).use { ps =>
        ps.stream(fragment.argument, 1024).compile.toList
      }
    } .onError {
      case NonFatal(e) => Sync[F].delay(e.printStackTrace())
    }

  }

}
