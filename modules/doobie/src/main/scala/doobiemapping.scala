// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package doobie

import java.sql.ResultSet

import cats.implicits._
import _root_.doobie.{ ConnectionIO, Fragment, Fragments, Get, Read }
import _root_.doobie.implicits._
import _root_.doobie.enum.Nullability._

import DoobieMapping._, FieldMapping._
import DoobiePredicate._
import Predicate._
import Query._
import ScalarType._

trait DoobieMapping {
  val objectMappings: List[ObjectMapping]

  def mkSelects(path: List[String]): Query =
    path.foldRight(Empty: Query) { (fieldName, child) => Select(fieldName, Nil, child) }

  def mapQuery(q: Query, tpe: Type): MappedQuery = {
    type Acc = (List[(Type, FieldMapping)], List[(Type, Predicate)])
    def loop(q: Query, tpe: Type, acc: Acc): Acc =
      q match {
        case Select(fieldName, _, q) =>
          val obj = tpe.underlyingObject
          (for {
            om      <- objectMappings.find(_.tpe =:= obj)
            (_, cr) <- om.fieldMappings.find(_._1 == fieldName)
          } yield loop(q, obj.underlyingField(fieldName), ((obj, cr) :: acc._1, acc._2))).getOrElse(acc)
        case Wrap(_, q) =>
          loop(q, tpe, acc)
        case Group(queries) =>
          queries.foldLeft(acc) {
            case (acc, sibling) => loop(sibling, tpe, acc)
          }
        case Filter(pred, child) =>
          val obj = tpe.underlyingObject
          val mc = loop(child, tpe, (acc._1, (obj, pred) :: acc._2))
          loop(mkSelects(pred.path), tpe, mc)
        case Unique(pred, child) =>
          val obj = tpe.underlyingObject
          val mc = loop(child, tpe, (acc._1, (obj, pred) :: acc._2))
          loop(mkSelects(pred.path), tpe, mc)
        case (_: Component) | (_: Defer) => acc
        case Empty => acc
      }

    val (mappings, predicates) = loop(q, tpe, (Nil, Nil))

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
    predicates: List[(Type, Predicate)],
    joins: List[Join],
    mapping: DoobieMapping
  ) {
    override def toString: String = {
      columns.toString
    }

    def index(col: ColumnRef): Int =
      columns.indexOf(col)

    def project(row: Row, cols: List[ColumnRef]): Row =
      Row(cols.map(cr => row(index(cr))))

    def select(row: Row, col: ColumnRef): Any =
      row(index(col))

    def hasField(tpe: Type, fieldName: String): Boolean = {
      val obj = tpe.dealias
      val om = mapping.objectMappings.find(_.tpe =:= obj).get
      om.fieldMappings.exists(_._1 == fieldName)
    }

    def columnOfField(tpe: Type, fieldName: String): ColumnRef = {
      val obj = tpe.dealias
      val om = mapping.objectMappings.find(_.tpe =:= obj).get
      val Some((_, col: ColumnRef)) = om.fieldMappings.find(_._1 == fieldName)
      col
    }

    def selectField(row: Row, tpe: Type, fieldName: String): Any = {
      val obj = tpe.dealias
      val om = mapping.objectMappings.find(_.tpe =:= obj).get
      val Some((_, col: ColumnRef)) = om.fieldMappings.find(_._1 == fieldName)
      select(row, col)
    }

    def hasKey(tpe: Type, keyName: String): Boolean = {
      val obj = tpe.dealias
      val om = mapping.objectMappings.find(_.tpe =:= obj).get
      om.key.exists(_.column == keyName)
    }

    def columnOfKey(tpe: Type, keyName: String): ColumnRef = {
      val obj = tpe.dealias
      val om = mapping.objectMappings.find(_.tpe =:= obj).get
      val Some(col: ColumnRef) = om.key.find(_.column == keyName)
      col
    }

    def selectKey(row: Row, tpe: Type, keyName: String): Any = {
      val obj = tpe.dealias
      val om = mapping.objectMappings.find(_.tpe =:= obj).get
      val Some(col) = om.key.find(_.column == keyName)
      select(row, col)
    }

    def hasSubobject(tpe: Type): Boolean = {
      val obj = tpe.dealias
      mapping.objectMappings.exists(_.tpe =:= obj)
    }

    def group(table: Table, cols: List[ColumnRef]): List[Table] =
      table.groupBy(row => project(row, cols)).to(List).sortBy(_._1.toString).map(_._2)

    def group(table: Table, tpe: Type): List[Table] =
      mapping.objectMappings.find(_.tpe =:= tpe) match {
        case Some(om) if om.key.nonEmpty => group(table, om.key)
        case None => table.map(List(_))
      }

    def rootCursorType(rootType: Type): Type = {
      def loop(tpe: Type): Type =
        tpe match {
          case tpe@ListType(_) => tpe
          case NullableType(tpe) => NullableType(loop(tpe))
          case tpe => ListType(tpe)
        }

      loop(rootType)
    }

    def fetch: ConnectionIO[Table] =
      fragment.query[Row](Row.mkRead(columns)).to[List]

    lazy val fragment: Fragment = {
      val cols = columns.map(_.toSql)
      val preds = predicates.map {
        case (tpe, FieldEquals(fieldName, value)) =>
          val col = columnOfField(tpe, fieldName)
          Fragment.const(s"${col.toSql} = ") ++ fr"$value"

        case (tpe, FieldLike(fieldName, pattern, caseInsensitive)) =>
          val col = columnOfField(tpe, fieldName)
          val op = if(caseInsensitive) "ILIKE" else "LIKE"
          Fragment.const(s"${col.toSql} $op ") ++ fr"$pattern"

        case (tpe, AttrEquals(keyName, value)) =>
          val col = columnOfKey(tpe, keyName)
          Fragment.const(s"${col.toSql} = ") ++ fr"$value"

        case (tpe, AttrLike(keyName, pattern, caseInsensitive)) =>
          val col = columnOfField(tpe, keyName)
          val op = if(caseInsensitive) "ILIKE" else "LIKE"
          Fragment.const(s"${col.toSql} $op ") ++ fr"$pattern"

        case (tpe, FieldContains(path, value)) =>
          val tpe1 = tpe.path(path.init).underlyingObject
          val col = columnOfField(tpe1, path.last)
          Fragment.const(s"${col.toSql} = ") ++ fr"$value"

        case _ => Fragment.empty
      }

      val where = Fragments.whereAnd(preds ++ joins.map(join => Fragment.const(join.toSql)): _*)

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
