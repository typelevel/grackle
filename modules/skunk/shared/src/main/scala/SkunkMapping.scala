// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package grackle
package skunk

import scala.util.control.NonFatal

import cats.{Foldable, Reducible}
import cats.effect.Sync
import cats.implicits._
import _root_.skunk.{ AppliedFragment, Decoder, Session, Fragment => SFragment }
import _root_.skunk.data.Arr
import _root_.skunk.codec.all.{ bool, varchar, int4, float8 }
import _root_.skunk.implicits._
import org.tpolecat.sourcepos.SourcePos
import org.tpolecat.typename.TypeName

import grackle.sql._
import grackle.sqlpg._

abstract class SkunkMapping[F[_]](
  val session: Session[F],
  val monitor: SkunkMonitor[F]
)(
  implicit val M: Sync[F]
) extends Mapping[F] with SkunkMappingLike[F]

trait SkunkMappingLike[F[_]] extends Mapping[F] with SqlPgMappingLike[F] { outer =>
  implicit val M: Sync[F]

  val session: Session[F]
  val monitor: SkunkMonitor[F]

  // Grackle needs to know about codecs, encoders, and fragments.
  type Codec    = (_root_.skunk.Codec[_], Boolean)
  type Encoder  = _root_.skunk.Encoder[_]
  type Fragment = _root_.skunk.AppliedFragment

  def toEncoder(c: Codec): Encoder = c._1
  def isNullable(c: Codec): Boolean = c._2

  // Also we need to know how to encode the basic GraphQL types.
  def booleanEncoder = bool
  def stringEncoder  = varchar
  def doubleEncoder  = float8
  def intEncoder     = int4

  def intCodec       = (int4, false)

  sealed trait IsNullable[T] {
    def isNullable: Boolean
  }
  object IsNullable extends IsNullable0 {
    implicit def nullable[T]: IsNullable[Option[T]] = new IsNullable[Option[T]] { def isNullable = true }
  }
  trait IsNullable0 {
    implicit def notNullable[T]: IsNullable[T] = new IsNullable[T] { def isNullable = false }
  }

  sealed trait NullableTypeName[T] {
    def value: String
  }
  object NullableTypeName extends NullableTypeName0 {
    implicit def nullable[T](implicit typeName: TypeName[T]): NullableTypeName[Option[T]] = new NullableTypeName[Option[T]] { def value = typeName.value }
  }
  trait NullableTypeName0 {
    implicit def notNullable[T](implicit typeName: TypeName[T]): NullableTypeName[T] = new NullableTypeName[T] { def value = typeName.value }
  }

  def col[T](colName: String, codec: _root_.skunk.Codec[T])(implicit tableName: TableName, typeName: NullableTypeName[T], isNullable: IsNullable[T], pos: SourcePos): ColumnRef =
    ColumnRef(tableName.name, colName, (codec, isNullable.isNullable), typeName.value, pos)

  // We need to demonstrate that our `Fragment` type has certain compositional properties.
  implicit def Fragments: SqlFragment[AppliedFragment] =
    new SqlFragment[AppliedFragment] { self =>
      def combine(x: AppliedFragment, y: AppliedFragment) = x |+| y
      def empty = AppliedFragment.empty

      // N.B. the casts here are due to a bug in the Scala 3 sql interpreter, caused by something
      // that may or may not be a bug in dotty. https://github.com/lampepfl/dotty/issues/12343
      def bind[A](encoder: Encoder, value: A) = sql"$encoder".asInstanceOf[SFragment[A]].apply(value)
      def in[G[_]: Reducible, A](f: AppliedFragment, fs: G[A], enc: Encoder): AppliedFragment = fs.toList.map(sql"$enc".asInstanceOf[SFragment[A]].apply).foldSmash(f |+| void" IN (", void", ", void")")(self, Foldable[List])

      def const(s: String) = sql"#$s".apply(_root_.skunk.Void)
      def and(fs: AppliedFragment*): AppliedFragment = fs.toList.map(parentheses).intercalate(void" AND ")(self)
      def andOpt(fs: Option[AppliedFragment]*): AppliedFragment = and(fs.toList.unite: _*)
      def or(fs: AppliedFragment*): AppliedFragment = fs.toList.map(parentheses).intercalate(void" OR ")(self)
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
    val rowDecoder: Decoder[Array[Any]] =
      new Decoder[Array[Any]] {
        val types = codecs.flatMap { case (_, (d, _)) => d.types }

        def arrToList(arr: Arr[_]): List[Any] =
          (arr.foldLeft(List.empty[Any]) { case (acc, elem) => elem :: acc }).reverse

        def decode(start: Int, ss: List[Option[String]]): Either[Decoder.Error, Array[Any]] = {
          val ncols = ss.length-start
          val arr = scala.Array.ofDim[Any](ncols)

          var i = 0
          var ss0 = ss.drop(start)
          var codecs0 = codecs
          while(i < ncols) {
            val (isJoin, (decoder, isNullable)) = codecs0.head
            val len = decoder.length
            val (seg, tl) = ss0.splitAt(len)
            val elem: Either[Decoder.Error, Any] =
              if(isJoin && !isNullable)
                decoder.opt.decode(0, seg).map(_.getOrElse(FailedJoin))
              else
                decoder.decode(0, seg)

            elem match {
              case Left(err) => return Left(err)
              case Right(v) =>
                v match {
                  case a: Arr[a] => arr(i) = arrToList(a)
                  case Some(a: Arr[a]) => arr(i) = Some(arrToList(a))
                  case other => arr(i) = other
                }
            }

            i = i + 1
            ss0 = tl
            codecs0 = codecs0.tail
          }

          Right(arr)
        }
      }

    session.prepare(fragment.fragment.query(rowDecoder)).flatMap { pq =>
      pq.stream(fragment.argument, 1024)
      .compile
      .toVector
    }
    .onError {
      case NonFatal(e) => Sync[F].delay(e.printStackTrace())
    }
  }
}
