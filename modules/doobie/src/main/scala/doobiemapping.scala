// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package doobie

import java.sql.ResultSet

import cats.implicits._
import cats.kernel.Monoid
import _root_.doobie.{ ConnectionIO, Fragment, Fragments, Get, Read }
import _root_.doobie.implicits._
import _root_.doobie.enum.Nullability._
import _root_.doobie.util.fragment.Elem.Arg
import _root_.doobie.util.meta.Meta
import io.circe.Encoder

import QueryCompiler._, ComponentElaborator.TrivialJoin
import DoobieMapping._, FieldMapping._
import DoobiePredicate._
import Predicate._
import Query._
import ScalarType._
import QueryInterpreter.mkErrorResult

trait DoobieMapping {
  val objectMappings: List[ObjectMapping]
  val leafMappings: List[LeafMapping[_]]

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

  def leafMapping[T](tpe: Type): Option[LeafMapping[T]] =
    leafMappings.collectFirst { case lm if lm.tpe =:= tpe => lm.asInstanceOf[LeafMapping[T]] }

  def typeToGet(tpe: Type): (Get[_], NullabilityKnown) =
    leafMapping[Any](tpe).map(lm => (lm.meta.get, NoNulls)).getOrElse(
      tpe match {
        case NullableType(tpe) => (typeToGet(tpe)._1, Nullable)
        case IntType => (Get[Int], NoNulls)
        case FloatType => (Get[Double], NoNulls)
        case StringType => (Get[String], NoNulls)
        case BooleanType => (Get[Boolean], NoNulls)
        case IDType => (Get[String], NoNulls)
        case _ => sys.error(s"no Get instance for schema type $tpe") // FIXME
      }
    )

  def attributeMapping(tpe: Type, attrName: String): Option[AttributeMapping] =
    for {
      om <- objectMapping(tpe)
      am <- om.attributeMapping(attrName)
    } yield am

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

  def primaryColumnForField(tpe: Type, fieldName: String): Option[ColumnRef] = {
    val obj = tpe.underlyingObject
    fieldMapping(obj, fieldName) match {
      case Some(cr: ColumnRef) => Some(cr)
      case _ => None
    }
  }

  def primaryColumnForTerm[T](tpe: Type, term: Term[T]): Option[ColumnRef] =
    term match {
      case Const(_) => None
      case path: Path =>
        val obj = tpe.underlyingObject
        val prefix = path.path.init
        val parent = obj.path(prefix)
        val name = path.path.last
        if (isField(path))
          primaryColumnForField(parent, name)
        else
          columnForAttribute(parent, name)
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
    type Acc = (List[ColumnRef], List[Join], List[(Type, Predicate)], List[(ObjectMapping, Type)])
    implicit object MAcc extends Monoid[Acc] {
      def combine(x: Acc, y: Acc): Acc =
        (x._1 ++ y._1, x._2 ++ y._2, x._3 ++ y._3, x._4 ++ y._4)

      def empty: Acc =  (Nil, Nil, Nil, Nil)
    }

    def loop(q: Query, tpe: Type, acc: Acc): Acc = {
      val obj = tpe.underlyingObject

      def loopPredicate(pred: Predicate): Acc = {
        def loopPath(path: Path): Acc = {
          def mkSelects(path: List[String]): Query =
            path.foldRight(Empty: Query) { (fieldName, child) => Select(fieldName, Nil, child) }

          val prefix = path.path.init
          val parent = obj.path(prefix)
          val name = path.path.last

          if (isField(path)) {
            loop(mkSelects(path.path), obj, acc)
          } else {
            val pcol = columnForAttribute(parent, name).getOrElse(sys.error(s"No attribute '$name' for type $parent"))
            val keyCols = keyColumnsForType(obj)
            val omt = objectMapping(obj).map(om => (om, obj)).toList
            (pcol :: keyCols, List.empty[Join], List.empty[(Type, Predicate)], omt) |+| loop(mkSelects(prefix), obj, acc)
          }
        }

        paths(pred).foldMap(loopPath) |+|
          ((List.empty[ColumnRef], List.empty[Join], List((tpe, pred)), List.empty[(ObjectMapping, Type)]))
      }

      q match {
        case Select(fieldName, _, child) =>
          val fieldTpe = obj.field(fieldName)
          val keyCols = keyColumnsForType(obj)
          val cols = columnsForField(obj, fieldName).toList ++ keyCols
          val joins = joinsForField(obj, fieldName)
          val omt = objectMapping(obj).map(om => (om, obj)).toList
          loop(child, fieldTpe, (cols, joins, List.empty[(Type, Predicate)], omt) |+| acc)

        case Narrow(subtpe, child) =>
          loop(child, subtpe, acc)
        case Filter(pred, child) =>
          loop(child, obj, loopPredicate(pred) |+| acc)
        case Unique(pred, child) =>
          loop(child, obj, loopPredicate(pred) |+| acc)
        case Wrap(_, q) =>
          loop(q, obj, acc)
        case Rename(_, q) =>
          loop(q, obj, acc)
        case Group(queries) =>
          queries.foldLeft(acc) {
            case (acc, sibling) => loop(sibling, obj, acc)
          }
        case GroupList(queries) =>
          queries.foldLeft(acc) {
            case (acc, sibling) => loop(sibling, obj, acc)
          }
        case Empty | (_: Component) | (_: Introspection) | (_: Defer) | (_: UntypedNarrow) | (_: Skip) => acc
      }
    }

    val (columns0, joins0, predicates, mappings0) = loop(q, tpe, MAcc.empty)

    val columns = columns0.distinct
    val mappings = mappings0.toMap
    val joins = joins0.distinctBy(_.normalize)

    val tables = columns.map(_.table).distinct
    val childTables = joins.map(_.child.table).toSet
    val rootTable = tables.filterNot(childTables) match {
      case List(rt) => rt
      case otherwise => sys.error(s"Expected unique root table, found $otherwise")
    }

    val orderedJoins = {
      def orderJoins(seen: Set[String], joins: List[Join], acc: List[Join]): List[Join] = {
        if (joins.isEmpty) acc
        else {
          val (admissable, rest) = joins.partition(j => seen(j.parent.table))
          if (admissable.isEmpty) sys.error(s"unable to order joins $joins given $seen")
          val ats = admissable.map(_.child.table)
          orderJoins(seen ++ ats, rest, admissable ++ acc)
        }
      }

      orderJoins(Set(rootTable), joins, Nil).reverse
    }

    val gets = {
      def getForColumn(col: ColumnRef): (Boolean, (Get[_], NullabilityKnown)) = {
        // A column is the product of an outer join (and may therefore be null even if it's non-nullable
        // in the schema) if its table introduced on the child side of a `Join`.
        def isJoin(cr: ColumnRef): Boolean = childTables(cr.table)

        object Target {
          def unapply(om: ObjectMapping): Option[(Boolean, (Get[_], NullabilityKnown))] =
            om.fieldMappings.collectFirst {
              case (fieldName, `col`) if mappings.contains(om) =>
                val get = typeToGet(mappings(om).field(fieldName))
                (isJoin(col), get)
            } orElse {
              om.attributeMappings.collectFirst {
                case (_, AttributeMapping(`col`, get)) =>
                  (isJoin(col), (get, NoNulls)) // support nullable attributes?
              }
            }
        }

        (objectMappings.collectFirst {
          case Target(ij, get) => (ij, get)
        }).getOrElse(sys.error(s"No Get for $col"))
      }

      columns.map(getForColumn)
    }

    new MappedQuery(rootTable, columns, gets, predicates, orderedJoins, this)
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
      def swap: Join = Join(child, parent)

      def toSql: String = s"LEFT JOIN ${child.table} ON ${parent.toSql} = ${child.toSql}"
    }
  }

  class LeafMapping[T](val tpe: Type, val encoder: Encoder[T], val meta: Meta[T])
  object LeafMapping {
    def apply[T](tpe: Type)(implicit encoder: Encoder[T], meta: Meta[T]) =
      new LeafMapping(tpe, encoder, meta)
  }

  case class MappedQuery(
    table: String,
    columns: List[ColumnRef],
    gets: List[(Boolean, (Get[_], NullabilityKnown))],
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
      mapping.attributeMapping(obj, attrName).map(_ => true).getOrElse(false)
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

    def fragmentForPred(tpe: Type, pred: Predicate): Fragment = {
      def term[T](x: Term[T], pt: Type): Fragment =
        x match {
          case path: Path =>
            mapping.primaryColumnForTerm(tpe, path) match {
              case Some(col) => Fragment.const(s"${col.toSql}")
              case None => Fragment.empty
            }
          case Const(value: String) => fr0"$value"
          case Const(value: Int) => fr0"$value"
          case Const(value: Double) => fr0"$value"
          case Const(value: Boolean) => fr0"$value"
          case Const(value) =>
            mapping.leafMapping[Any](pt) match {
              case Some(lm) =>
                Fragment("?", List(Arg[Any](value, lm.meta.put)), None)
              case _ =>
                Fragment.empty
            }
        }

      def unify[T](x: Term[T], y: Term[T]): Type =
        (x, y) match {
          case (path: Path, _) => tpe.path(path.path)
          case (_, path: Path) => tpe.path(path.path)
          case _ => NoType
        }

      def loop(pred: Predicate): Fragment =
        pred match {
          case And(x, y) => Fragments.and(loop(x), loop(y))
          case Or(x, y) => Fragments.or(loop(x), loop(y))
          case Not(x) => fr"NOT" ++ loop(x)
          case Eql(x, y) =>
            val pt = unify(x, y)
            term(x, pt) ++ fr0" = "++ term(y, pt)
          case Contains(x, y) =>
            val pt = unify(x, y)
            term(x, pt) ++ fr0" = "++ term(y, pt)
          case Lt(x, y) =>
            val pt = unify(x, y)
            term(x, pt) ++ fr0" < "++ term(y, pt)
          case Like(x, pattern, caseInsensitive) =>
            val op = if(caseInsensitive) "ILIKE" else "LIKE"
            term(x, StringType) ++ Fragment.const(s" $op ") ++ fr0"$pattern"
          case _ => Fragment.empty
        }

      loop(pred)
    }

    lazy val fragment: Fragment = {
      val cols = columns.map(_.toSql)

      val preds = predicates.map((fragmentForPred _).tupled)
      val where = Fragments.whereAnd(preds: _*)

      val select =
        Fragment.const0(
          s"""
          |SELECT ${cols.mkString(", ")}
          |FROM $table${if (joins.isEmpty) "" else joins.map(_.toSql).mkString("\n", "\n", "")}
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

    override def transform(query: Query, env: Env, schema: Schema, tpe: Type): Result[Query] = {
      def loop(query: Query, tpe: Type, filtered: Set[Type]): Result[Query] = {
        query match {
          case s@Select(fieldName, _, child) =>
            tpe.withUnderlyingField(fieldName) { childTpe =>
              if(filtered(childTpe.underlyingObject)) {
                val elaboratedSelect = loop(child, childTpe, Set.empty).map(ec => s.copy(child = ec))
                elaboratedSelect.map(ec => Wrap(fieldName, Defer(stagingJoin, ec)))
              } else {
                loop(child, childTpe, filtered + tpe.underlyingObject).map(ec => s.copy(child = ec))
              }
            }

          case n@Narrow(subtpe, child) => loop(child, subtpe, filtered).map(ec => n.copy(child = ec))
          case w@Wrap(_, child)        => loop(child, tpe, filtered).map(ec => w.copy(child = ec))
          case r@Rename(_, child)      => loop(child, tpe, filtered).map(ec => r.copy(child = ec))
          case g@Group(queries)        => queries.traverse(q => loop(q, tpe, filtered)).map(eqs => g.copy(queries = eqs))
          case g@GroupList(queries)    => queries.traverse(q => loop(q, tpe, filtered)).map(eqs => g.copy(queries = eqs))
          case u@Unique(_, child)      => loop(child, tpe.nonNull, filtered + tpe.underlyingObject).map(ec => u.copy(child = ec))
          case f@Filter(_, child)      => loop(child, tpe.item, filtered + tpe.underlyingObject).map(ec => f.copy(child = ec))
          case c: Component            => c.rightIor
          case i: Introspection        => i.rightIor
          case d: Defer                => d.rightIor
          case Empty                   => Empty.rightIor

          case s: Skip                 => mkErrorResult(s"Unexpected Skip ${s.render}")
          case n: UntypedNarrow        => mkErrorResult(s"Unexpected UntypeNarrow ${n.render}")
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
  // Placeholder for nulls read from non-nullable columns introduced via an outer join.
  case object FailedJoin

  def mkRead(gets: List[(Boolean, (Get[_], NullabilityKnown))]): Read[Row] = {
    def unsafeGet(rs: ResultSet, n: Int): Row =
      Row {
        gets.zipWithIndex.map {
          case ((isJoin, (g, NoNulls)),  i) =>
            if (isJoin) g.unsafeGetNullable(rs, n+i).getOrElse(FailedJoin)
            else g.unsafeGetNonNullable(rs, n+i)
          case ((_, (g, Nullable)), i) => g.unsafeGetNullable(rs, n+i)
        }
      }

    new Read(gets.map(_._2), unsafeGet)
  }
}
