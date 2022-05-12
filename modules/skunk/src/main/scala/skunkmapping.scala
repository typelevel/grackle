// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package skunk

import scala.util.control.NonFatal

import cats.Reducible
import cats.effect.{ Resource, Sync }
import cats.implicits._
import _root_.skunk.{ AppliedFragment, Decoder, Session, Fragment => SFragment }
import _root_.skunk.codec.all.{ bool, varchar, int4, float8 }
import _root_.skunk.implicits._
import org.tpolecat.sourcepos.SourcePos
import org.tpolecat.typename.TypeName

import edu.gemini.grackle.sql._

abstract class SkunkMapping[F[_]: Sync](
  val pool:    Resource[F, Session[F]],
  val monitor: SkunkMonitor[F]
) extends SqlMapping[F] { outer =>

  // Grackle needs to know about codecs, encoders, and fragments.
  type Codec    = (_root_.skunk.Codec[_], Boolean)
  type Encoder  = _root_.skunk.Encoder[_]
  type Fragment = _root_.skunk.AppliedFragment

  def toEncoder(c: Codec): Encoder = c._1

  // Also we need to know how to encode the basic GraphQL types.
  def booleanEncoder = bool
  def stringEncoder  = varchar
  def doubleEncoder  = float8
  def intEncoder     = int4

  def intCodec       = (int4, false)

  class TableDef(name: String) {
    sealed trait IsNullable[T] {
      def isNullable: Boolean
    }
    object IsNullable extends IsNullable0 {
      implicit def nullable[T]: IsNullable[Option[T]] = new IsNullable[Option[T]] { def isNullable = true }
    }
    trait IsNullable0 {
      implicit def notNullable[T]: IsNullable[T] = new IsNullable[T] { def isNullable = false }
    }

    def col[T](colName: String, codec: _root_.skunk.Codec[T])(implicit typeName: TypeName[T], isNullable: IsNullable[T], pos: SourcePos): ColumnRef =
      ColumnRef(name, colName, (codec, isNullable.isNullable), typeName.value, pos)
  }

  // We need to demonstrate that our `Fragment` type has certain compositional properties.
  implicit def Fragments: SqlFragment[AppliedFragment] =
    new SqlFragment[AppliedFragment] {
      def combine(x: AppliedFragment, y: AppliedFragment) = x |+| y
      def empty = AppliedFragment.empty

      // N.B. the casts here are due to a bug in the Scala 3 sql interpreter, caused by something
      // that may or may not be a bug in dotty. https://github.com/lampepfl/dotty/issues/12343
      def bind[A](encoder: Encoder, value: A) = sql"$encoder".asInstanceOf[SFragment[A]].apply(value)
      def in[G[_]: Reducible, A](f: AppliedFragment, fs: G[A], enc: Encoder): AppliedFragment = fs.toList.map(sql"$enc".asInstanceOf[SFragment[A]].apply).foldSmash(f |+| void" IN (", void", ", void")")

      def const(s: String) = sql"#$s".apply(_root_.skunk.Void)
      def and(fs: AppliedFragment*): AppliedFragment = fs.toList.map(parentheses).intercalate(void" AND ")
      def andOpt(fs: Option[AppliedFragment]*): AppliedFragment = and(fs.toList.unite: _*)
      def or(fs: AppliedFragment*): AppliedFragment = fs.toList.map(parentheses).intercalate(void" OR ")
      def orOpt(fs: Option[AppliedFragment]*): AppliedFragment = or(fs.toList.unite: _*)
      def whereAnd(fs: AppliedFragment*): AppliedFragment = if (fs.isEmpty) AppliedFragment.empty else void"WHERE " |+| and(fs: _*)
      def whereAndOpt(fs: Option[AppliedFragment]*): AppliedFragment = whereAnd(fs.toList.unite: _*)
      def parentheses(f: AppliedFragment): AppliedFragment = void"(" |+| f |+| void")"

      def needsCollation(codec: Codec): Boolean =
        codec._1.types.head.name match {
          case "char" => true
          case "character" => true
          case "character varying" => true
          case "text" => true
          case "varchar" => true
          case _ => false
        }

      def sqlTypeName(codec: Codec): Option[String] =
        Some(codec._1.types.head.name)
    }

  // And we need to be able to fetch `Rows` given a `Fragment` and a list of decoders.
  def fetch(fragment: Fragment, codecs: List[(Boolean, Codec)]): F[Vector[Array[Any]]] = {
    lazy val rowDecoder: Decoder[Array[Any]] =
      new Decoder[Array[Any]] {

        lazy val types = codecs.flatMap { case (_, (d, _)) => d.types }

        lazy val decodersWithOffsets: List[(Boolean, Decoder[_], Int)] =
          codecs.foldLeft((0, List.empty[(Boolean, Decoder[_], Int)])) {
            case ((offset, accum), (isJoin, (decoder, isNullable))) =>
              (offset + decoder.length, (isJoin && !isNullable, decoder, offset) :: accum)
          } ._2.reverse

        def decode(start: Int, ssx: List[Option[String]]): Either[Decoder.Error, Array[Any]] = {
          val ss = ssx.drop(start)
          decodersWithOffsets.traverse {

            // If the column is the outer part of a join and it's a non-nullable in the schema then
            // we read it as an option and collapse it, using FailedJoin in the None case. Otherwise
            // read as normal.
            case (true,  c, offset) => c.opt.decode(0, ss.drop(offset).take(c.length)).map(_.getOrElse(FailedJoin))
            case (false, c, offset) => c    .decode(0, ss.drop(offset).take(c.length))
          }.map(_.toArray)
        }
      }

    pool.use { s =>
      s.prepare(fragment.fragment.query(rowDecoder)).use { ps =>
        ps.stream(fragment.argument, 1024).compile.toVector
      }
    } .onError {
      case NonFatal(e) => Sync[F].delay(e.printStackTrace())
    }

  }

}
