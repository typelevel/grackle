// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package doobie.postgres

import java.sql.ResultSet

import cats.Reducible
import cats.effect.Sync
import _root_.doobie.{ Meta, Put, Read, Transactor, Fragment => DoobieFragment }
import _root_.doobie.enumerated.JdbcType._
import _root_.doobie.enumerated.Nullability.{ NoNulls, Nullable }
import _root_.doobie.implicits._
import _root_.doobie.util.fragments
import org.tpolecat.sourcepos.SourcePos
import org.tpolecat.typename.TypeName

import edu.gemini.grackle.sql._

abstract class DoobieMapping[F[_]](
  val transactor: Transactor[F],
  val monitor:    DoobieMonitor[F],
)(
  implicit val M: Sync[F]
) extends Mapping[F] with DoobieMappingLike[F]

trait DoobieMappingLike[F[_]] extends Mapping[F] with SqlMappingLike[F] {
  implicit val M: Sync[F]

  def transactor: Transactor[F]
  def monitor:    DoobieMonitor[F]

  type Codec   = (Meta[_], Boolean)
  type Encoder = (Put[_], Boolean)
  type Fragment   = DoobieFragment

  def toEncoder(c: Codec): Encoder = (c._1.put, c._2)

  def intCodec       = (Meta[Int], false)
  def intEncoder     = (Put[Int], false)
  def stringEncoder  = (Put[String], false)
  def booleanEncoder = (Put[Boolean], false)
  def doubleEncoder  = (Put[Double], false)

  def col[T](colName: String, codec: Meta[T], nullable: Boolean = false)(implicit tableName: TableName, typeName: TypeName[T], pos: SourcePos): ColumnRef =
    ColumnRef(tableName.name, colName, (codec, nullable), typeName.value, pos)

  implicit def Fragments: SqlFragment[Fragment] =
    new SqlFragment[Fragment] {
      def combine(x: Fragment, y: Fragment): Fragment = x ++ y
      def empty: Fragment = DoobieFragment.empty
      def bind[A](encoder: Encoder, value: A): Fragment = {
        val (e0, nullable) = encoder
        implicit val e = e0.asInstanceOf[Put[A]]
        if (!nullable) sql"$value"
        else
          value match {
            case None    => sql"NULL"
            case Some(v) => sql"${v.asInstanceOf[A]}"
            case v       => sql"${v.asInstanceOf[A]}"
          }
      }
      def const(s: String): Fragment = DoobieFragment.const(s)
      def and(fs: Fragment*): Fragment = fragments.and(fs: _*)
      def andOpt(fs: Option[Fragment]*): Fragment = fragments.andOpt(fs: _*)
      def orOpt(fs: Option[Fragment]*): Fragment = fragments.orOpt(fs: _*)
      def whereAnd(fs: Fragment*): Fragment = fragments.whereAnd(fs: _*)
      def whereAndOpt(fs: Option[Fragment]*): Fragment = fragments.whereAndOpt(fs: _*)
      def parentheses(f: Fragment): Fragment = fragments.parentheses(f)
      def in[G[_]: Reducible, A](f: Fragment, fs: G[A], enc: Encoder): Fragment = {
        val (put, _) = enc
        fragments.in(f, fs)(implicitly, put.asInstanceOf[Put[A]])
      }

      def needsCollation(codec: Codec): Boolean =
        codec._1.put.jdbcTargets.head match {
          case Char                  => true
          case Clob                  => true
          case LongnVarChar          => true
          case LongVarChar           => true
          case NChar                 => true
          case NClob                 => true
          case NVarChar              => true
          case VarChar               => true
          case _                     => false
        }

      def sqlTypeName(codec: Codec): Option[String] =
        codec._1.put.jdbcTargets.head match {
          case BigInt                => Some("BIGINT")
          case Binary                => Some("BINARY")
          case Bit                   => Some("BOOLEAN")
          case Blob                  => Some("BLOB")
          case Boolean               => Some("BOOLEAN")
          case Char                  => Some("CHAR")
          case Clob                  => Some("CLOB")
          case DataLink              => Some("DATALINK")
          case Date                  => Some("DATE")
          case Decimal               => Some("DECIMAL")
          case Distinct              => Some("DISTINCT")
          case Double                => Some("DOUBLE")
          case Float                 => Some("FLOAT")
          case Integer               => Some("INTEGER")
          case JavaObject            => Some("JAVA_OBJECT")
          case LongnVarChar          => Some("LONGNVARCHAR")
          case LongVarBinary         => Some("LONGVARBINARY")
          case LongVarChar           => Some("LONGVARCHAR")
          case NChar                 => Some("NCHAR")
          case NClob                 => Some("NCLOB")
          case Null                  => Some("NULL")
          case Numeric               => Some("NUMERIC")
          case NVarChar              => Some("NVARCHAR")
          case Real                  => Some("REAL")
          case Ref                   => Some("REF")
          case RefCursor             => Some("REF CURSOR")
          case RowId                 => Some("ROWID")
          case SmallInt              => Some("SMALLINT")
          case SqlXml                => Some("XML")
          case Struct                => Some("STRUCT")
          case Time                  => Some("TIME")
          case TimeWithTimezone      => Some("TIME WITH TIMEZONE")
          case Timestamp             => Some("TIMESTAMP")
          case TimestampWithTimezone => Some("TIMESTAMP WITH TIMEZONE")
          case TinyInt               => Some("TINYINT")
          case VarBinary             => Some("VARBINARY")
          case VarChar               => Some("VARCHAR")
          case Array | Other         =>
            codec._1.put match {
              case adv: Put.Advanced[_] if adv.schemaTypes.head == "json" =>
                Some("JSONB")
              case adv: Put.Advanced[_] =>
                Some(adv.schemaTypes.head)
              case _ => None
            }

          case _                     => None
        }
    }

  def fetch(fragment: Fragment, codecs: List[(Boolean, Codec)]): F[Vector[Array[Any]]] = {
    val ncols = codecs.length

    def mkRead(codecs: List[(Boolean, Codec)]): Read[Array[Any]] = {
      def unsafeGet(rs: ResultSet, n: Int): Array[Any] = {
        val arr = scala.Array.ofDim[Any](ncols)
        var i = 0
        var codecs0 = codecs
        while(i < ncols) {
          codecs0.head match {
            case (isJoin, (m, false)) =>
              if (isJoin) arr(i) = m.get.unsafeGetNullable(rs, n+i).getOrElse(FailedJoin)
              else arr(i) = m.get.unsafeGetNonNullable(rs, n+i)
            case (_, (m, true)) => arr(i) = m.get.unsafeGetNullable(rs, n+i)
          }
          i = i + 1
          codecs0 = codecs0.tail
        }

        arr
      }

      new Read(codecs.map { case (_, (m, n)) => (m.get, if(n) Nullable else NoNulls) }, unsafeGet)
    }

    fragment.query[Array[Any]](mkRead(codecs)).to[Vector].transact(transactor)
  }
}
