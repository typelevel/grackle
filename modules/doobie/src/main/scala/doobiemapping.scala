// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package doobie

import java.sql.ResultSet

import cats.implicits._
import _root_.doobie.{ ConnectionIO, Fragment, Fragments, Get, Read }
import _root_.doobie.enum.Nullability._

import DoobieMapping._, FieldMapping._
import Query._
import ScalarType._

trait DoobieMapping {
  val objectMappings: List[ObjectMapping]

  def mapQuery(q: Query, tpe: Type, predicates: List[Fragment]): MappedQuery = {
    def loop(q: Query, tpe: Type, acc: List[(Type, FieldMapping)]): List[(Type, FieldMapping)] = q match {
      case Select(name, _, q) =>
        val obj = tpe.underlyingObject
        (for {
          om      <- objectMappings.find(_.tpe =:= obj)
          (_, cr) <- om.fieldMappings.find(_._1 == name)
        } yield loop(q, obj.underlyingField(name), (obj, cr) :: acc)).getOrElse(acc)
      case Group(queries) =>
        queries.foldLeft(acc) {
          case (acc, sibling) => loop(sibling, tpe, acc)
        }
      case Empty => acc
    }

    val mappings: List[(Type, FieldMapping)] = loop(q, tpe, Nil)

    val types: List[Type] = mappings.map(_._1).distinct

    val keys: List[ColumnRef] = types.flatMap { tpe =>
      val obj = tpe.underlyingObject
      objectMappings.find(_.tpe =:= obj).map(_.key).getOrElse(Nil)
    }

    val columns: List[ColumnRef] = (mappings.foldLeft(keys) {
      case (acc, (_, cr: ColumnRef)) => cr :: acc
      case (acc, (_, Subobject(_, joins))) => joins.map(_.parent) ++ acc
      case (acc, _) => acc
    }).distinct

    val tables: List[String] = columns.map(_.table).distinct

    val joins: List[Join] = (mappings.foldLeft(List.empty[Join]) {
      case (acc, (_, Subobject(_, joins))) => joins ++ acc
      case (acc, _) => acc
    }).distinctBy(_.normalize)

    new MappedQuery(tables, columns, predicates, joins, this)
  }
}

object DoobieMapping {
  case class ObjectMapping(
    tpe: Type,
    key: List[ColumnRef],
    fieldMappings: List[(String, FieldMapping)]
  )

  sealed trait FieldMapping
  object FieldMapping {
    case class ColumnRef(table: String, column: String, tpe: Type) extends FieldMapping {
      def toSql: String = s"$table.$column"
    }
    case class Subobject(tpe: Type, joins: List[Join])  extends FieldMapping

    case class Join(parent: ColumnRef, child: ColumnRef) {
      def normalize: Join = {
        if (parent.table > child.table) this
        else if (parent.table == child.table && parent.column >= child.column) this
        else Join(child, parent)
      }

      def toSql: String = s"${parent.toSql} = ${child.toSql}"
    }
  }

  case class MappedQuery(
    tables: List[String],
    columns: List[ColumnRef],
    predicates: List[Fragment],
    joins: List[Join],
    mapping: DoobieMapping
  ) {
    def index(col: ColumnRef): Int =
      columns.indexOf(col)

    def project(row: Row, cols: List[ColumnRef]): Row =
      Row(cols.map(cr => row(index(cr))))

    def select(row: Row, col: ColumnRef): Any =
      row(index(col))

    def select(row: Row, tpe: Type, field: String): Any = {
      val obj = tpe.dealias
      val om = mapping.objectMappings.find(_.tpe =:= obj).get
      val Some((_, col: ColumnRef)) = om.fieldMappings.find(_._1 == field)
      select(row, col)
    }

    def group(table: Table, cols: List[ColumnRef]): List[Table] =
      table.groupBy(row => project(row, cols)).to(List).sortBy(_._1.toString).map(_._2)

    def group(table: Table, tpe: Type): List[Table] =
      mapping.objectMappings.find(_.tpe =:= tpe) match {
        case Some(om) if om.key.nonEmpty => group(table, om.key)
        case None => table.map(List(_))
      }

    def fetch: ConnectionIO[Table] =
      fragment.query[Row](Row.mkRead(columns)).to[List]

    lazy val fragment: Fragment = {
      val cols = columns.map(_.toSql)
      val where = Fragments.whereAnd(predicates ++ joins.map(join => Fragment.const(join.toSql)): _*)

      val select =
        Fragment.const(
          s"""
          |SELECT ${cols.mkString(", ")}
          |FROM ${tables.mkString(", ")}
          |""".stripMargin
        )

      (select ++ where)
    }
  }
}

case class Row(elems: List[Any]) {
  def apply(i: Int): Any = elems(i)
}

object Row {
  def mkRead(cols: List[ColumnRef]): Read[Row] = {
    def typeToGet(tpe: Type): Get[_] = tpe match {
      case IntType => Get[String]
      case FloatType => Get[Double]
      case StringType => Get[String]
      case BooleanType => Get[Boolean]
      case _ => Get[String]
    }

    val gets = cols.map(col => (typeToGet(col.tpe), NoNulls))

    def unsafeGet(rs: ResultSet, n: Int): Row = {
      Row(gets.zipWithIndex.map { case (g, i) => g._1.unsafeGetNonNullable(rs, n+i) })
    }

    new Read(gets, unsafeGet)
  }
}
