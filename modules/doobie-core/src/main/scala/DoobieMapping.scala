// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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
package doobie

import cats.Reducible
import cats.effect.Sync
import _root_.doobie.{ Meta, Put, Read, Transactor, Fragment => DoobieFragment }
import _root_.doobie.enumerated.JdbcType._
import _root_.doobie.implicits._
import _root_.doobie.util.fragments
import org.tpolecat.sourcepos.SourcePos
import org.tpolecat.typename.TypeName

import grackle.sql._

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
  def isNullable(c: Codec): Boolean = c._2

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
      def and(fs: Fragment*): Fragment = fragments.andOpt(fs).getOrElse(empty)
      def andOpt(fs: Option[Fragment]*): Fragment = fragments.andOpt(fs.flatten).getOrElse(empty)
      def orOpt(fs: Option[Fragment]*): Fragment = fragments.orOpt(fs.flatten).getOrElse(empty)
      def whereAnd(fs: Fragment*): Fragment = fragments.whereAndOpt(fs)
      def whereAndOpt(fs: Option[Fragment]*): Fragment = fragments.whereAndOpt(fs.flatten)
      def parentheses(f: Fragment): Fragment = fragments.parentheses(f)
      def in[G[_]: Reducible, A](f: Fragment, fs: G[A], enc: Encoder): Fragment = {
        val (put, _) = enc
        implicit val putA: Put[A] = put.asInstanceOf[Put[A]]
        fragments.inOpt(f, fs).getOrElse(empty)
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

      def sqlTypeName(codec: Codec): Option[String] = {
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
          case TimeWithTimezone      => Some("TIME WITH TIME ZONE")
          case Timestamp             => Some("TIMESTAMP")
          case TimestampWithTimezone => Some("TIMESTAMP WITH TIME ZONE")
          case TinyInt               => Some("TINYINT")
          case VarBinary             => Some("VARBINARY")
          case VarChar               => Some("VARCHAR")
          case Array | Other         =>
            codec._1.put.vendorTypeNames.headOption match {
              case Some("json") => Some("JSONB")
              case other => other
            }

          case _                     => None
        }
      }
    }

  def fetch(fragment: Fragment, codecs: List[(Boolean, Codec)]): F[Result[Vector[Array[Any]]]] = {
    import cats.syntax.all._
    
    val reads: Array[Read[Any]] = codecs.toArray.map {
      case (isJoin, (m, false)) =>
        if (isJoin) 
          new Read.SingleOpt(m.get.widen[Any]).map(_.getOrElse(FailedJoin))
        else 
          new Read.Single(m.get.widen[Any])
      case (_, (m, true))     =>
        (new Read.SingleOpt(m.get.widen[Any]): Read[Option[Any]]).widen[Any]
    }
    
    implicit val read: Read[Array[Any]] = new Read.CompositeOfInstances[Any](reads)
    fragment.query[Array[Any]].to[Vector].transact(transactor).map(Result.success)
  }
}
