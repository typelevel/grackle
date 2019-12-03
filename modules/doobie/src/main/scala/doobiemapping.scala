// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package doobie

import java.sql.ResultSet

import cats.implicits._
import _root_.doobie.{ ConnectionIO, Fragment, Fragments, Get, Read }
import _root_.doobie.implicits._
import _root_.doobie.enum.Nullability._

import QueryCompiler._, ComponentElaborator.TrivialJoin
import DoobieMapping._, FieldMapping._
import DoobiePredicate._
import Predicate._
import Query._
import ScalarType._
import QueryInterpreter.mkErrorResult

trait DoobieMapping {
  val objectMappings: List[ObjectMapping]

  def objectMapping(tpe: Type): Option[ObjectMapping] = {
    tpe match {
      case nt: NamedType => objectMappings.find(_.tpe == nt.name)
      case _ => None
    }
  }

  def fieldMapping(tpe: Type, fieldName: String): Option[FieldMapping] =
    for {
      om <- objectMapping(tpe)
      fm <- om.fieldMapping(fieldName)
    } yield fm

  def attributeMapping(tpe: Type, attrName: String): Option[AttributeMapping] =
    for {
      om <- objectMapping(tpe)
      am <- om.attributeMapping(attrName)
    } yield am

  def getForColumn(mappings: Map[ObjectMapping, Type], col: ColumnRef): Option[Get[_]] = {
    object Target {
      def unapply(om: ObjectMapping): Option[Get[_]] =
        om.fieldMappings.collectFirst {
          case (fieldName, `col`) if mappings.contains(om) => Row.typeToGet(mappings(om).field(fieldName))
        } orElse {
          om.attributeMappings.collectFirst {
            case (_, AttributeMapping(`col`, get)) => get
          }
        }
    }
    objectMappings.collectFirst {
      case Target(get) => get
    }
  }

  def columnsForField(tpe: Type, fieldName: String): List[ColumnRef] = {
    val obj = tpe.underlyingObject
    fieldMapping(obj, fieldName) match {
      case Some(cr: ColumnRef) => List(cr)
      case Some(Subobject(joins, _)) => joins.map(_.parent) ++ joins.map(_.child)
      case _ => Nil
    }
  }

  def joinsForField(tpe: Type, fieldName: String): List[Join] = {
    val obj = tpe.underlyingObject
    fieldMapping(obj, fieldName) match {
      case Some(Subobject(joins, _)) => joins
      case _ => Nil
    }
  }

  def columnForAttribute(tpe: Type, attrName: String): Option[ColumnRef] = {
    val obj = tpe.underlyingObject
    attributeMapping(obj, attrName) match {
      case Some(AttributeMapping(cr, _)) => Some(cr)
      case _ => None
    }
  }

  def columnsJoinsForPredicate(tpe: Type, pred: Predicate): Option[(List[ColumnRef], List[Join], ColumnRef)] = {
    def loop(tpe: Type, names: List[String], cols0: List[ColumnRef], joins0: List[Join]): Option[(List[ColumnRef], List[Join], ColumnRef)] = {
      val obj = tpe.underlyingObject
      val keyCols = keyColumnsForType(obj)
      (names: @unchecked) match {
        case name :: Nil =>
          for {
            col <- if (pred.isField) columnsForField(obj, name).headOption else columnForAttribute(obj, name)
          } yield (col :: keyCols ++ cols0, joins0, col)
        case fieldName :: tl =>
          loop(obj.field(fieldName), tl,
            columnsForField(obj, fieldName).toList ++ keyCols ++ cols0,
            joinsForField(obj, fieldName) ++ joins0
          )
      }
    }

    loop(tpe, pred.path, Nil, Nil)
  }

  def keyColumnsForType(tpe: Type): List[ColumnRef] = {
    val obj = tpe.underlyingObject
    objectMapping(obj) match {
      case Some(om) => om.key
      case _ => Nil
    }
  }

  // This is partial, however, we should be able to perform a consistency check ahead of time
  // such that a valid query is guaranteed to be covered.
  def mapQuery(q: Query, tpe: Type): MappedQuery = {
    type Acc = (List[ColumnRef], List[Join], List[(ColumnRef, Predicate)], List[(ObjectMapping, Type)])
    def loop(q: Query, tpe: Type, acc: Acc): Acc = {
      val keyCols = keyColumnsForType(tpe)
      val omt = objectMapping(tpe.underlyingObject).map(om => (om, tpe)).toList
      q match {
        case Select(fieldName, _, child) =>
          val fieldTpe = tpe.underlyingField(fieldName)
          loop(child, fieldTpe, (
            columnsForField(tpe, fieldName).toList ++ keyCols ++ acc._1,
            joinsForField(tpe, fieldName) ++ acc._2,
            acc._3,
            omt ++ acc._4
          ))
        case Filter(pred, child) =>
          val Some((cols, joins, col)) = columnsJoinsForPredicate(tpe, pred) // FIXME
          loop(child, tpe, (cols ++ acc._1, joins ++ acc._2, (col, pred) :: acc._3, omt ++ acc._4))
        case Unique(pred, child) =>
          val Some((cols, joins, col)) = columnsJoinsForPredicate(tpe, pred) // FIXME
          loop(child, tpe, (cols ++ acc._1, joins ++ acc._2, (col, pred) :: acc._3, omt ++ acc._4))
        case Wrap(_, q) =>
          loop(q, tpe, acc)
        case Group(queries) =>
          queries.foldLeft(acc) {
            case (acc, sibling) => loop(sibling, tpe, acc)
          }
        case Empty | (_: Component) | (_: Defer) => acc
      }
    }

    val (columns0, joins0, predicates, mappings0) = loop(q, tpe, (Nil, Nil, Nil, Nil))

    val columns = columns0.distinct
    val tables = columns.map(_.table).distinct
    val mappings = mappings0.toMap
    val gets = columns.map(col => getForColumn(mappings, col).getOrElse(Get[String]))
    val joins = joins0.distinctBy(_.normalize)

    new MappedQuery(tables, columns, gets, predicates, joins, this)
  }
}

object DoobieMapping {
  case class ObjectMapping(
    tpe: String,
    key: List[ColumnRef],
    fieldMappings: List[(String, FieldMapping)],
    attributeMappings: List[(String, AttributeMapping)]
  ) {
    def fieldMapping(fieldName: String): Option[FieldMapping] =
      fieldMappings.find(_._1 == fieldName).map(_._2)

    def attributeMapping(attrName: String): Option[AttributeMapping] =
      attributeMappings.find(_._1 == attrName).map(_._2)
  }

  case class AttributeMapping(col: ColumnRef, get: Get[_])

  def Attr[T](col: ColumnRef)(implicit get: Get[T]): AttributeMapping =
    new AttributeMapping(col, get)

  sealed trait FieldMapping
  object FieldMapping {
    case class ColumnRef(table: String, column: String) extends FieldMapping {
      def toSql: String = s"$table.$column"
    }
    case class Subobject(joins: List[Join], stagingJoin: (Cursor, Query) => Result[Query] = TrivialJoin) extends FieldMapping

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
    gets: List[Get[_]],
    predicates: List[(ColumnRef, Predicate)],
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
      mapping.fieldMapping(obj, fieldName).map(_ => true).getOrElse(false)
    }

    def columnOfField(tpe: Type, fieldName: String): ColumnRef = {
      val obj = tpe.dealias
      val Some(col: ColumnRef) = mapping.fieldMapping(obj, fieldName)
      col
    }

    def selectField(row: Row, tpe: Type, fieldName: String): Any = {
      val obj = tpe.dealias
      val Some(col: ColumnRef) = mapping.fieldMapping(obj, fieldName)
      select(row, col)
    }

    def hasAttribute(tpe: Type, attrName: String): Boolean = {
      val obj = tpe.dealias
      mapping.fieldMapping(obj, attrName).map(_ => true).getOrElse(false)
    }

    def columnOfAttribute(tpe: Type, attrName: String): ColumnRef = {
      val obj = tpe.dealias
      val Some(col: ColumnRef) = mapping.attributeMapping(obj, attrName).map(_.col)
      col
    }

    def selectAttribute(row: Row, tpe: Type, attrName: String): Any = {
      val obj = tpe.dealias
      val Some(col: ColumnRef) = mapping.attributeMapping(obj, attrName).map(_.col)
      select(row, col)
    }

    def hasSubobject(tpe: Type): Boolean = {
      tpe.dealias match {
        case nt: NamedType => mapping.objectMappings.exists(_.tpe == nt.name)
        case _ => false
      }
    }

    def group(table: Table, cols: List[ColumnRef]): List[Table] =
      table.groupBy(row => project(row, cols)).to(List).sortBy(_._1.toString).map(_._2)

    def group(table: Table, tpe: Type): List[Table] =
      mapping.objectMapping(tpe) match {
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
      fragment.query[Row](Row.mkRead(gets)).to[List]

    lazy val fragment: Fragment = {
      val cols = columns.map(_.toSql)
      val preds = predicates.map {
        case (col, FieldEquals(_, value)) =>
          Fragment.const(s"${col.toSql} = ") ++ fr"$value"

        case (col, FieldLike(_, pattern, caseInsensitive)) =>
          val op = if(caseInsensitive) "ILIKE" else "LIKE"
          Fragment.const(s"${col.toSql} $op ") ++ fr"$pattern"

        case (col, AttrEquals(_, value)) =>
          Fragment.const(s"${col.toSql} = ") ++ fr"$value"

        case (col, AttrLike(_, pattern, caseInsensitive)) =>
          val op = if(caseInsensitive) "ILIKE" else "LIKE"
          Fragment.const(s"${col.toSql} $op ") ++ fr"$pattern"

        case (col, FieldContains(_, value)) =>
          Fragment.const(s"${col.toSql} = ") ++ fr"$value"

        case (col, AttrContains(_, value)) =>
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

  class StagingElaborator(mapping: DoobieMapping) extends Phase {
    val stagingJoin = (c: Cursor, q: Query) => q match {
      case Select(fieldName, _, _) =>
        val obj = c.tpe.underlyingObject

        val osj = mapping.fieldMapping(obj, fieldName).collect { case so: Subobject => so.stagingJoin }

        osj match {
          case Some(stagingJoin) => stagingJoin(c, q)
          case None =>
            mkErrorResult(s"No staging join for field '$fieldName' of type $obj")
        }
      case _ => mkErrorResult(s"No staging join for non-Select $q")
    }

    def apply(query: Query, tpe: Type): Result[Query] = {
      def loop(query: Query, tpe: Type, filtered: Set[Type]): Result[Query] = {
        query match {
          case s@Select(fieldName, _, child) =>
            val childTpe = tpe.underlyingField(fieldName)
            if(filtered(childTpe.underlyingObject)) {
              val elaboratedSelect = loop(child, childTpe, Set.empty).map(ec => s.copy(child = ec))
              elaboratedSelect.map(ec => Wrap(fieldName, Defer(stagingJoin, ec)))
            } else {
              loop(child, childTpe, filtered + tpe.underlyingObject).map(ec => s.copy(child = ec))
            }

          case w@Wrap(_, child)              => loop(child, tpe, filtered).map(ec => w.copy(child = ec))
          case g@Group(queries)              => queries.traverse(q => loop(q, tpe, filtered)).map(eqs => g.copy(queries = eqs))
          case u@Unique(_, child)            => loop(child, tpe.nonNull, filtered + tpe.underlyingObject).map(ec => u.copy(child = ec))
          case f@Filter(_, child)            => loop(child, tpe.item, filtered + tpe.underlyingObject).map(ec => f.copy(child = ec))
          case c: Component                  => c.rightIor
          case d: Defer                      => d.rightIor
          case Empty                         => Empty.rightIor
        }
      }

      loop(query, tpe, Set.empty)
    }
  }
}

case class Row(elems: List[Any]) {
  def apply(i: Int): Any = elems(i)
}

object Row {
  def typeToGet(tpe: Type): Get[_] = tpe match {
    case IntType => Get[Int]
    case FloatType => Get[Double]
    case StringType => Get[String]
    case BooleanType => Get[Boolean]
    case _ => Get[String]
  }

  def mkRead(gets0: List[Get[_]]): Read[Row] = {
    val gets = gets0.map(get => (get, NoNulls))

    def unsafeGet(rs: ResultSet, n: Int): Row = {
      Row(gets.zipWithIndex.map { case (g, i) => g._1.unsafeGetNonNullable(rs, n+i) })
    }

    new Read(gets, unsafeGet)
  }
}
