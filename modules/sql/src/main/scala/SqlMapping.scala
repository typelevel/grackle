// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package sql

import scala.annotation.tailrec

import cats.{Eq, Monoid}
import cats.data.{Chain, Ior, NonEmptyList}
import cats.implicits._
import io.circe.Json
import org.tpolecat.typename._
import org.tpolecat.sourcepos.SourcePos

import Cursor.Env
import Predicate._
import Query._
import QueryCompiler._
import QueryInterpreter.{mkErrorResult, ProtoJson}
import circe.CirceMapping
import sql._

/** An abstract mapping that is backed by a SQL database. */
trait SqlMapping[F[_]] extends CirceMapping[F] with SqlModule[F] { self =>

  override val validator: SqlMappingValidator =
    SqlMappingValidator(this)

  private def discriminator(om: ObjectMapping): List[ColumnRef] =
    om.fieldMappings.collect {
      case cm: SqlField if cm.discriminator => cm.columnRef
      case am: SqlAttribute if am.discriminator => am.col
    }

  private def key(om: ObjectMapping): List[ColumnRef] =
    om.fieldMappings.collect {
      case cm: SqlField if cm.key => cm.columnRef
      case am: SqlAttribute if am.key => am.col
    }

  private def isField(p: Path): Boolean =
    p match {
      case FieldPath(_) | CollectFieldPath(_) => true
      case _ => false
    }

  /**
   * Name of a SQL schema column and its associated codec. Note that `ColumnRef`s are consider equal
   * if their table and column names are equal.
   */
  case class ColumnRef(table: String, column: String, codec: Codec[_], scalaTypeName: String)(
    implicit val pos: SourcePos
  ) {

    /** This `ColumnRef` as a SQL expression of the form `table.column`. */
    def toSql: String =
      s"$table.$column"

    override def equals(other: Any) =
      other match {
        case ColumnRef(`table`, `column`, _, _) => true
        case _ => false
      }

    override def hashCode(): Int =
      table.hashCode() + column.hashCode()

  }

  object ColumnRef {

    def apply[A: TypeName](table: String, column: String, codec: Codec[A])(
      implicit pos: SourcePos
    ): ColumnRef =
      apply(table, column, codec, typeName)

  }

  /** A pair of `ColumnRef`s, representing a SQL join. `ColumnRef`s have a canonical form. */
  case class Join(parent: ColumnRef, child: ColumnRef) {

    def normalize: Join =
      if (parent.table > child.table) this
      else if (parent.table === child.table && parent.column >= child.column) this
      else Join(child, parent)

    def swap: Join =
      Join(child, parent)

    def toSql: String =
      s"LEFT JOIN ${child.table} ON ${parent.toSql} = ${child.toSql}"

    def toWhere: Fragment =
      Fragments.const(s"${parent.toSql} = ${child.toSql}")
  }

  case class SqlRoot(fieldName: String, path: List[String] = Nil, rootTpe: Type = NoType)(
    implicit val pos: SourcePos
  ) extends RootMapping {

    private def mkRootCursor(query: Query, fieldPath: List[String], fieldTpe: Type, env: Env): F[Result[Cursor]] = {
      val mapped = MappedQuery(query, fieldPath, fieldTpe)
      val cursorType = fieldTpe.list
      for {
        table <- mapped.fetch
        _     <- monitor.queryMapped(query, mapped.fragment, table)
      } yield SqlCursor(fieldPath, cursorType, table, mapped, None, env).rightIor
    }

    def cursor(query: Query, env: Env): F[Result[Cursor]] =
      if (fieldName === "__staged")
        mkRootCursor(query, path, rootTpe, env)
      else
        mkRootCursor(query, fieldName :: path, rootTpe.underlyingField(fieldName), env)

    def withParent(tpe: Type): SqlRoot =
      copy(rootTpe = tpe)

  }

  sealed trait SqlFieldMapping extends FieldMapping {
    final def isPublic = true
    final def withParent(tpe: Type): FieldMapping = this
  }

  case class SqlAttribute(
    fieldName: String,
    col: ColumnRef,
    key: Boolean = false,
    nullable: Boolean = false,
    discriminator: Boolean = false,
  )(implicit val pos: SourcePos) extends FieldMapping {
    def isPublic = false
    def withParent(tpe: Type): FieldMapping = this
  }

  case class SqlField(
    fieldName: String,
    columnRef: ColumnRef,
    key: Boolean = false,
    discriminator: Boolean = false
  )(implicit val pos: SourcePos) extends SqlFieldMapping

  case class SqlObject(fieldName: String, joins: List[Join])(
    implicit val pos: SourcePos
  ) extends SqlFieldMapping
  object SqlObject {
    def apply(fieldName: String, joins: Join*): SqlObject = apply(fieldName, joins.toList)
  }

  case class SqlJson(fieldName: String, columnRef: ColumnRef)(
    implicit val pos: SourcePos
  ) extends SqlFieldMapping

  sealed trait SqlInterfaceMapping extends ObjectMapping {
    def discriminate(cursor: Cursor): Result[Type]
  }

  object SqlInterfaceMapping {

   sealed abstract case class DefaultInterfaceMapping(tpe: Type, fieldMappings: List[FieldMapping], path: List[String])(
     implicit val pos: SourcePos
   ) extends SqlInterfaceMapping

    val defaultDiscriminator: Cursor => Result[Type] = (cursor: Cursor) => cursor.tpe.rightIor

    def apply(tpe: Type, fieldMappings: List[FieldMapping], path: List[String] = Nil, discriminator: Cursor => Result[Type] = defaultDiscriminator)(
      implicit pos: SourcePos
    ): ObjectMapping =
      new DefaultInterfaceMapping(tpe, fieldMappings.map(_.withParent(tpe)), path) {
        def discriminate(cursor: Cursor): Result[Type] = discriminator(cursor)
      }

  }

  sealed trait SqlUnionMapping extends ObjectMapping {
    def discriminate(cursor: Cursor): Result[Type]
  }

  object SqlUnionMapping {

    sealed abstract case class DefaultUnionMapping(tpe: Type, fieldMappings: List[FieldMapping], path: List[String])(
      implicit val pos: SourcePos
    ) extends SqlUnionMapping

    val defaultDiscriminator: Cursor => Result[Type] = (cursor: Cursor) => cursor.tpe.rightIor

    def apply(tpe: Type, fieldMappings: List[FieldMapping], path: List[String] = Nil, discriminator: Cursor => Result[Type] = defaultDiscriminator)(
      implicit pos: SourcePos
    ): ObjectMapping =
      new DefaultUnionMapping(tpe, fieldMappings.map(_.withParent(tpe)), path) {
        def discriminate(cursor: Cursor): Result[Type] = discriminator(cursor)
      }

  }

  final class MappedQuery(
    table: String,
    columns: List[ColumnRef],
    metas: List[(Boolean, (Codec[_], NullabilityKnown))],
    predicates: List[(List[String], Type, Predicate)],
    joins: List[Join],
    conditions: List[Fragment]
  ) {

    def fetch: F[Table] = self.fetch(fragment, metas)

    override def toString: String = {
      columns.toString
    }

    def index(col: ColumnRef): Int =
      columns.indexOf(col)

    private def project(row: Row, cols: List[ColumnRef]): Row =
      Row(cols.map(cr => row(index(cr))))

    private def select(row: Row, col: ColumnRef): Any =
      row(index(col))

    def selectField(row: Row, path: List[String], tpe: Type, fieldName: String): Result[Any] = {
      val obj = tpe.dealias
      fieldMapping(path, obj, fieldName) match {
        case Some(SqlField(_, col, _, _)) => select(row, col).rightIor
        case Some(SqlJson(_, col)) => select(row, col).rightIor
        case other => mkErrorResult(s"Expected mapping for field $fieldName of type $obj, found $other")
      }
    }

    def hasAttribute(path: List[String], tpe: Type, attrName: String): Boolean = {
      val obj = tpe.dealias
      fieldMapping(path, obj, attrName) match {
        case Some(_: SqlAttribute) => true
        case _ => false
      }
    }

    def selectAttribute(row: Row, path: List[String], tpe: Type, attrName: String): Result[Any] = {
      val obj = tpe.dealias
      fieldMapping(path, obj, attrName) match {
        case Some(SqlAttribute(_, col, _, _, _)) => select(row, col).rightIor
        case other => mkErrorResult(s"Expected mapping for attribute $attrName of type $obj, found $other")
      }
    }

    def stripNulls(table: Table, path: List[String], tpe: Type): Table =
      objectMapping(path, tpe.nonNull) match {
        case Some(om) if key(om).nonEmpty =>
          val cols = key(om)
          table.filterNot(row => project(row, cols).elems.exists(_ == FailedJoin))
        case _ => table
      }

    def narrowsTo(table: Table, path: List[String], tpe: Type): Boolean =
      objectMapping(path, tpe.nonNull) match {
        case Some(om) if key(om).nonEmpty =>
          val cols = key(om)
          !table.exists(row => project(row, cols).elems.exists(_ == FailedJoin))
        case _ => false
      }

    def group(table: Table, path: List[String], tpe: Type): List[Table] =
      objectMapping(path, tpe) match {
        case Some(om) if key(om).nonEmpty =>
          val cols = key(om)
          val nonNull = table.filterNot(row => project(row, cols).elems.exists(_ == FailedJoin))
          nonNull.groupBy(row => project(row, cols)).to(List).sortBy(_._1.toString).map(_._2)
        case _ => table.map(List(_))
      }


    private def fragmentForPred(path: List[String], tpe: Type, pred: Predicate): Option[Fragment] = {

      def termColumnForAttribute(path: List[String], tpe: Type, attrName: String): Option[ColumnRef] = {
        val obj = tpe.underlyingObject
        fieldMapping(path, obj, attrName) match {
          case Some(SqlAttribute(_, cr, _, _, _)) => Some(cr)
          case _ => None
        }
      }

      def termColumnForField(path: List[String], tpe: Type, fieldName: String): Option[ColumnRef] = {
        val obj = tpe.underlyingObject
        fieldMapping(path, obj, fieldName) match {
          case Some(SqlField(_, cr, _, _)) => Some(cr)
          case Some(SqlJson(_, cr)) => Some(cr)
          case _ => None
        }
      }

      def primaryColumnForTerm[T](path: List[String], tpe: Type, term: Term[T]): Option[ColumnRef] =
        term match {
          case Const(_) => None
          case termPath: Path =>
            val obj = tpe.underlyingObject
            val prefix = termPath.path.init
            val parent = obj.path(prefix)
            val name = termPath.path.last
            if (isField(termPath))
              termColumnForField(path.reverse_:::(termPath.path), parent, name)
            else
              termColumnForAttribute(path, parent, name)
        }

      def term[T](x: Term[T], put: Encoder[T], nullable: Boolean): Option[Fragment] =
        x match {
          case Const(value) => Some(Fragments.bind(put, nullable, value))
          case other =>
            primaryColumnForTerm(path, tpe, other).map(col => Fragments.const(col.toSql))
        }

      def putForTerm(x: Term[_]): Option[(Encoder[_], Boolean)] =
        x match {
          case path: Path =>
            primaryColumnForTerm(path.path, tpe, path).map(cr =>
              (toEncoder(cr.codec.asInstanceOf[Codec[Any]]), tpe.path(path.path).isNullable)
            )
          case (_: And)|(_: Or)|(_: Not)|(_: Eql[_])|(_: NEql[_])|(_: Lt[_])|(_: LtEql[_])|(_: Gt[_])|(_: GtEql[_])  => Some((booleanEncoder, false))
          case (_: AndB)|(_: OrB)|(_: XorB)|(_: NotB) => Some((intEncoder, false))
          case (_: ToUpperCase)|(_: ToLowerCase) => Some((stringEncoder, false))
          case _ => None
        }

      def loop[T](exp: Term[T], put: Option[(Encoder[T], Boolean)]): Option[Fragment] = {

        def unify(x: Term[_], y: Term[_]): Option[(Encoder[Any], Boolean)] =
          putForTerm(x).orElse(putForTerm(y)).orElse(put).asInstanceOf[Option[(Encoder[Any], Boolean)]]

        def nonEmpty(frag: Fragment): Option[Fragment] =
          if (frag == Fragments.empty) None
          else Some(frag)

        exp match {
          case Const(value) =>
            put.map { case (pa, nullable) => Fragments.bind(pa, nullable, value) }

          case Project(prefix, pred) =>
            fragmentForPred(prefix ++ path, tpe.path(prefix), pred)

          case pathTerm: Path =>
            primaryColumnForTerm(path, tpe, pathTerm).map(col => Fragments.const(col.toSql))

          case And(x, y) =>
            nonEmpty(Fragments.andOpt(loop(x, None), loop(y, None)))

          case Or(x, y) =>
            nonEmpty(Fragments.orOpt(loop(x, None), loop(y, None)))

          case Not(x) =>
            loop(x, Some((booleanEncoder, false))).map(x => Fragments.const("NOT ") |+| x)

          case Eql(x, y) =>
            val p = unify(x, y)
            for {
              x <- loop(x, p)
              y <- loop(y, p)
            } yield x |+| Fragments.const(" = ") |+| y

          case NEql(x, y) =>
            val p = unify(x, y)
            for {
              x <- loop(x, p)
              y <- loop(y, p)
            } yield x |+| Fragments.const(" != ")|+| y

          case Contains(x, y) =>
            val p = unify(x, y)
            for {
              x <- loop(x, None)
              y <- loop(y, p)
            } yield x |+| Fragments.const(" = ")|+| y

          case Lt(x, y) =>
            val p = unify(x, y)
            for {
              x <- loop(x, p)
              y <- loop(y, p)
            } yield x |+| Fragments.const(" < ") |+| y

          case LtEql(x, y) =>
            val p = unify(x, y)
            for {
              x <- loop(x, p)
              y <- loop(y, p)
            } yield x |+| Fragments.const(" <= ")|+| y

          case Gt(x, y) =>
            val p = unify(x, y)
            for {
              x <- loop(x, p)
              y <- loop(y, p)
            } yield x |+| Fragments.const(" > ")|+| y
          case GtEql(x, y) =>
            val p = unify(x, y)
            for {
              x <- loop(x, p)
              y <- loop(y, p)
            } yield x |+| Fragments.const(" >= ")|+| y

          case In(x, y) =>
            for {
              p0 <- putForTerm(x)
              (p, n) = p0.asInstanceOf[(Encoder[Any], Boolean)]
              x <- term(x, p, n)
              l <- NonEmptyList.fromList(y)
            } yield Fragments.in(x, l, p)

          case AndB(x, y) =>
            for {
              x <- term(x, intEncoder, false)
              y <- term(y, intEncoder, false)
            } yield x |+| Fragments.const(" & ")|+| y
          case OrB(x, y) =>
            for {
              x <- term(x, intEncoder, false)
              y <- term(y, intEncoder, false)
            } yield x |+| Fragments.const(" | ")|+| y
          case XorB(x, y) =>
            for {
              x <- term(x, intEncoder, false)
              y <- term(y, intEncoder, false)
            } yield x |+| Fragments.const(" # ")|+| y

          case NotB(x) =>
            loop(x, Some((intEncoder, false))).map(x => Fragments.const("~") |+| x)

          case StartsWith(x, prefix) =>
            for {
              x <- term(x, stringEncoder, false)
            } yield x |+| Fragments.const(" LIKE ") |+| Fragments.bind(stringEncoder, false, prefix + "%")

          case ToUpperCase(x) =>
            loop(x, Some((stringEncoder, false))).map(x => Fragments.const("upper(") |+| x |+| Fragments.const(")"))

          case ToLowerCase(x) =>
            loop(x, Some((stringEncoder, false))).map(x => Fragments.const("lower(") |+| x |+| Fragments.const(")"))

          case Like(x, pattern, caseInsensitive) =>
            val op = if(caseInsensitive) "ILIKE" else "LIKE"
            term(x, stringEncoder, false).map(x => x |+| Fragments.const(s" $op ") |+| Fragments.bind(stringEncoder, false, pattern))

          case _ =>
            None
        }
      }

      loop(pred, None)
    }

    lazy val fragment: Fragment = {
      val cols = columns.map(_.toSql)

      val preds = predicates.map((fragmentForPred _).tupled) ++ conditions.map(Some(_))
      val where = Fragments.whereAndOpt(preds: _*)

      val select = Fragments.const(
        s"""|SELECT ${cols.mkString(", ")}
            |FROM $table${if (joins.isEmpty) "" else joins.map(_.toSql).mkString("\n", "\n", "")}
            |""".stripMargin
      )

      (select |+| where)
    }
  }


  object MappedQuery {

    // This is partial, however, we should be able to perform a consistency check ahead of time
    // such that a valid query is guaranteed to be covered.
    def apply(q: Query, path: List[String], tpe: Type): MappedQuery = {

      type Acc = (List[ColumnRef], List[Join], List[(List[String], Type, Predicate)])
      implicit object MonoidAcc extends Monoid[Acc] {
        def combine(x: Acc, y: Acc): Acc = (x._1 ++ y._1, x._2 ++ y._2, x._3 ++ y._3)
        def empty: Acc =  (Nil, Nil, Nil)
      }

      def discriminatorColumnsForType(path: List[String], tpe: Type): List[ColumnRef] = {
        val obj = tpe.underlyingObject
        objectMapping(path, obj) match {
          case Some(om) => discriminator(om)
          case _ => Nil
        }
      }

      def keyColumnsForType(path: List[String], tpe: Type): List[ColumnRef] = {
        val obj = tpe.underlyingObject
        objectMapping(path, obj) match {
          case Some(om) => key(om)
          case _ => Nil
        }
      }

      def paths(t: Term[_]): List[Path] =
        t match {
          case Project(prefix, x) => paths(x).map(_.extend(prefix))
          case p: Path => List(p)
          case And(x, y) => paths(x) ++ paths(y)
          case Or(x, y) => paths(x) ++ paths(y)
          case Not(x) => paths(x)
          case Eql(x, y) => paths(x) ++ paths(y)
          case NEql(x, y) => paths(x) ++ paths(y)
          case Contains(x, y) => paths(x) ++ paths(y)
          case Lt(x, y) => paths(x) ++ paths(y)
          case LtEql(x, y) => paths(x) ++ paths(y)
          case Gt(x, y) => paths(x) ++ paths(y)
          case GtEql(x, y) => paths(x) ++ paths(y)
          case In(x, _) => paths(x)
          case AndB(x, y) => paths(x) ++ paths(y)
          case OrB(x, y) => paths(x) ++ paths(y)
          case XorB(x, y) => paths(x) ++ paths(y)
          case NotB(x) => paths(x)
          case StartsWith(x, _) => paths(x)
          case ToUpperCase(x) => paths(x)
          case ToLowerCase(x) => paths(x)
          case Matches(x, _) => paths(x)
          case Like(x, _, _) => paths(x)
          case _ => Nil
        }


      def columnsForFieldOrAttribute(path: List[String], tpe: Type, name: String): List[ColumnRef] = {
        val obj = tpe.underlyingObject
        fieldMapping(path, obj, name) match {
          case Some(SqlField(_, cr, _, _)) => List(cr)
          case Some(SqlJson(_, cr)) => List(cr)
          case Some(SqlObject(_, joins)) => joins.map(_.parent) ++ joins.map(_.child)
          case Some(SqlAttribute(_, cr, _, _, _)) => List(cr)
          case Some(CursorField(_, _, _, required)) =>
            required.flatMap(r => columnsForFieldOrAttribute(path, tpe, r))
          case Some(CursorAttribute(_, _, required)) =>
            required.flatMap(r => columnsForFieldOrAttribute(path, tpe, r))
          case _ => Nil
        }
      }

      def loop(q: Query, path: List[String], tpe: Type, acc: Acc): Acc = {
        val obj = tpe.underlyingObject
        lazy val interfaces =
          obj.underlyingObject match {
            case ObjectType(_, _, _, interfaces) => interfaces
            case _ => Nil
          }

        def requiredCols: List[ColumnRef] =
          discriminatorColumnsForType(path, obj) ++ interfaces.flatMap(discriminatorColumnsForType(path, _)) ++ keyColumnsForType(path, obj)

        def joinsForField(path: List[String], tpe: Type, fieldName: String): List[Join] = {
          val obj = tpe.underlyingObject
          fieldMapping(path, obj, fieldName) match {
            case Some(SqlObject(_, joins)) => joins
            case _ => Nil
          }
        }

        def loopPath(term: Path): Acc = {
          def mkSelects(path: List[String]): Query =
            path.foldRight(Empty: Query) { (fieldName, child) => Select(fieldName, Nil, child) }

          val prefix = term.path.init
          val parent = obj.path(prefix)
          val name = term.path.last

          if (isField(term)) {
            loop(mkSelects(term.path), path, obj, acc)
          } else {
            columnsForFieldOrAttribute(path, parent, name) match {
              case Nil => loop(mkSelects(prefix), path, obj, acc)
              case pcols =>
                (pcols ++ requiredCols, List.empty[Join], List.empty[(List[String], Type, Predicate)]) |+| loop(mkSelects(prefix), path, obj, acc)
            }
          }
        }

        def loopPredicate(pred: Predicate): Acc =
          paths(pred).foldMap(loopPath) |+|
            ((List.empty[ColumnRef], List.empty[Join], List((path, tpe, pred))))

        def loopOrderSelections(oss: OrderSelections): Acc =
          oss.selections.flatMap(os => paths(os.term)).foldMap(loopPath)

        q match {
          case Select(fieldName, _, child) =>
            val fieldTpe = obj.field(fieldName)
            val joins = joinsForField(path, obj, fieldName)
            val joinCols = joins.flatMap(j => List(j.parent, j.child))
            val cols = columnsForFieldOrAttribute(path, obj, fieldName).toList ++ requiredCols ++ joinCols

            loop(child, fieldName :: path, fieldTpe, (cols, joins, List.empty[(List[String], Type, Predicate)]) |+| acc)

          case _: Introspect =>
            ((requiredCols, Nil, Nil): Acc) |+| acc

          case Context(contextPath, child) =>
            loop(child, contextPath, tpe, acc)

          case Environment(_, child) =>
            loop(child, path, tpe, acc)

          case Narrow(subtpe, child) =>
            loop(child, path, subtpe, (requiredCols, List.empty[Join], List.empty[(List[String], Type, Predicate)]) |+| acc)
          case Filter(pred, child) =>
            loop(child, path, obj, loopPredicate(pred) |+| acc)
          case Unique(pred, child) =>
            loop(child, path, obj, loopPredicate(pred) |+| acc)
          case Wrap(_, q) =>
            loop(q, path, obj, acc)
          case Rename(_, q) =>
            loop(q, path, obj, acc)
          case Group(queries) =>
            queries.foldLeft(acc) {
              case (acc, sibling) => loop(sibling, path, obj, acc)
            }
          case GroupList(queries) =>
            queries.foldLeft(acc) {
              case (acc, sibling) => loop(sibling, path, obj, acc)
            }
          case Limit(_, child) =>
            loop(child, path, obj, acc)
          case OrderBy(oss, child) =>
            loop(child, path, obj, loopOrderSelections(oss) |+| acc)

          case GroupBy(_, child) =>
            loop(child, path, obj, acc)

          case Empty | Skipped | Query.Component(_, _, _) | (_: Defer) | (_: UntypedNarrow) | (_: Skip) => acc
        }
      }

      val (columns0, joins0, predicates) = loop(q, path, tpe, MonoidAcc.empty)

      val columns = columns0.distinct
      val joins = joins0.distinctBy(_.normalize)

      def numChildren(t: String): Int =
        joins.filter(_.parent.table === t).distinctBy(_.child.table).size

      val tables = columns.map(_.table).distinct
      val childTables = joins.map(_.child.table).toSet
      val rootTable = tables.filterNot(childTables) match {
        case List(rt) => rt
        case _ => tables.maxBy(numChildren)
      }

      val orderedJoins0 = {
        def orderJoins(seen: Set[String], joins: List[Join], acc: List[Join]): List[Join] = {
          if (joins.isEmpty) acc
          else {
            val (admissable, rest) = joins.partition(j => seen(j.parent.table))
            if (admissable.isEmpty) sys.error(s"unable to order joins $joins given $seen")
            val ats = admissable.map(_.child.table).toSet
            orderJoins(ats, rest, admissable ++ acc)
          }
        }

        orderJoins(Set(rootTable), joins, Nil).reverse
      }

      // Currently the SQL compiler doesn't generate subqueries in where clauses, and so projected
      // predicates (which would naturally compile to subqueries in where clauses) end up being
      // compiled as joins. Where there is more than one of these, or where one of these is
      // combined with a legitimate join, we end up generating multiple left joins with the same
      // child table. Postgres rejects this.
      //
      // This is a workaround for that, in advance of a reworking of the compiler which
      // generates subqueries, which checks for duplicate joins and converts all but the first to
      // where clauses.
      //
      // This is not valid in general, but works for some currently critical cases of projected
      // predicates. It should be removed as soon as projected predicates are compiled more
      // sensibly.

      def extractDuplicates(joins: List[Join]): (List[Join], List[Join]) = {
        @tailrec
        def loop(joins: List[Join], js: List[Join], ws: List[Join]): (List[Join], List[Join]) =
          joins match {
            case Nil => (js.reverse, ws.reverse)
            case hd :: tl =>
              if(js.exists(_.child.table == hd.child.table)) loop(tl, js, hd :: ws)
              else loop(tl, hd :: js, ws)
          }
        loop(joins, Nil, Nil)
      }

      val (orderedJoins, conditions) = extractDuplicates(orderedJoins0)

      val metas = {
        def metaForColumn(col: ColumnRef): (Boolean, (Codec[_], NullabilityKnown)) = {
          def loop(tms: List[TypeMapping]): Option[(Codec[_], NullabilityKnown)] =
            tms match {
              case Nil => None
              case tm :: tl =>
                tm match {

                  case om: ObjectMapping =>
                    om.fieldMappings.collectFirst {

                      case SqlField(fieldName, `col`, _, _) =>
                        val obj = om.tpe.underlyingObject
                        val fieldTpe0 = obj.field(fieldName)
                        val fieldTpe =
                          if (obj.variantField(fieldName)) fieldTpe0.nullable
                          else fieldTpe0
                        fieldTpe match {
                          case NullableType(_) => (col.codec, Nullable)
                          case _ => (col.codec, NoNulls)
                        }

                      case SqlJson(fieldName, `col`) =>
                        val obj = om.tpe.underlyingObject
                        val nullable = obj.field(fieldName).isNullable || obj.variantField(fieldName)
                        (col.codec, if (nullable) Nullable else NoNulls)

                      case SqlAttribute(_, `col`, _, nullable, _) =>
                        (col.codec, if (nullable) Nullable else NoNulls)

                    } orElse loop(tl)

                  case pm: PrefixedMapping =>
                    loop(pm.mappings.map(_._2)).orElse(loop(tl))

                  case _ =>
                    loop(tl)

                }
            }

          // A column is the product of an outer join (and may therefore be null even if it's non-nullable
          // in the schema) if its table introduced on the child side of a `Join`.
          def isJoin(cr: ColumnRef): Boolean = childTables(cr.table)

          loop(typeMappings).map(mn => (isJoin(col), mn)).getOrElse(sys.error(s"No Codec for $col"))
        }

        columns.map(metaForColumn)
      }

      new MappedQuery(rootTable, columns, metas, predicates, orderedJoins, conditions.map(_.toWhere))
    }

  }

  object StagingElaborator extends Phase {

    val stagingJoin = (c: Cursor, q: Query) =>
      q match {
        case Select(_, _, _) =>
          val obj = c.tpe.underlyingObject
          val Some(om) = objectMapping(c.path, obj)

          def predForCursor(c: Cursor): Result[Query] = {
            val pred =
              Predicate.and(
                om.fieldMappings.collect {
                  case cm: SqlField if cm.key =>
                    val fv0 = c.field(cm.fieldName)
                    val Ior.Right(fv) = fv0
                    Eql(FieldPath(List(cm.fieldName)), Const(fv.focus))(Eq.fromUniversalEquals)

                  case am: SqlAttribute if am.key =>
                    val av0 = c.attribute(am.fieldName)
                    val Ior.Right(av) = av0
                    Eql(AttrPath(List(am.fieldName)), Const(av))(Eq.fromUniversalEquals)
                }
              )
            Context(c.path, Select("__staged", Nil, Filter(pred, q))).rightIor
          }

          if (c.isNullable)
            c.asNullable match {
              case Ior.Right(Some(c)) => predForCursor(c)
              case _ => Empty.rightIor
            }
          else predForCursor(c)


        case _ => mkErrorResult(s"No staging join for non-Select $q")
      }

    override def transform(query: Query, vars: Vars, schema: Schema, tpe: Type): Result[Query] = {
      def nonLeafList(path: List[String], tpe: Type, fieldName: String): Boolean = {
        val fieldTpe = tpe.underlyingField(fieldName).nonNull
        fieldTpe.isList &&
          (fieldMapping(path, tpe.underlyingObject, fieldName) match {
            case Some(SqlObject(_, joins)) if joins.nonEmpty => true
            case _ => false
          })
      }

      case class Seen[T](context: T, seenTypes: Set[ObjectMapping], seenList: Boolean) {
        def map[U](f: T => U): Seen[U] = copy(context = f(context))
        def withQuery(q: Query): Seen[Query] = copy(context = q)
        def withType(q: Query, path: List[String], tpe: Type): Seen[Query] =
          objectMapping(path, tpe.underlyingObject).map(om =>
            copy(context = q, seenTypes = seenTypes + om)
          ).getOrElse(copy(context = q))
        def hasSeen(path: List[String], tpe: Type): Boolean = {
          objectMapping(path, tpe.underlyingObject).map(seenTypes).getOrElse(false)
        }
        def withList(q: Query, seen: => Boolean) = copy(context = q, seenList = seenList || seen)
        def forGroup: Seen[List[Query]] = copy(context = List.empty)
      }

      object Seen {
        def apply(q: Query): Seen[Query] = new Seen(q, Set.empty, false)
      }

      def hasDiscriminator(path: List[String], tpe: Type): Boolean = {
        val obj = tpe.underlyingObject
        objectMapping(path, obj) match {
          case Some(om) => discriminator(om).nonEmpty
          case _ => false
        }
      }

      def loop(path: List[String], tpe: Type, seen: Seen[Query]): Result[Seen[Query]] = {
        seen.context match {
          case s@Select(fieldName, _, child) =>
            tpe.withUnderlyingField(fieldName) { childTpe =>
              if(seen.hasSeen(fieldName :: path, childTpe.underlyingObject) || (seen.seenList && nonLeafList(path, tpe, fieldName))) {
                val elaboratedSelect = loop(fieldName :: path, childTpe, Seen(child)).map(ec => s.copy(child = ec.context))
                elaboratedSelect.map(ec => seen.withQuery(Wrap(fieldName, Defer(stagingJoin, ec, tpe.underlyingObject))))
              } else if(childTpe.dealias.isInterface && childTpe.variantField(fieldName) && !hasDiscriminator(path, childTpe)) {
                val elaboratedSelect = loop(fieldName :: path, childTpe, Seen(child)).map(ec => s.copy(child = ec.context))
                elaboratedSelect.map(ec => seen.withQuery(Wrap(fieldName, Defer(stagingJoin, ec, schema.queryType))))
              } else {
                val elaboratedSelect = loop(fieldName :: path, childTpe, seen.withType(child, fieldName :: path, tpe))
                elaboratedSelect.map(ec => ec.withList(s.copy(child = ec.context), nonLeafList(path, tpe, fieldName)))
              }
            }

          case c@Context(cPath, child) => loop(cPath, tpe, seen.withQuery(child)).map(_.map(q => c.copy(child = q)))
          case e@Environment(_, child) => loop(path, tpe, seen.withQuery(child)).map(_.map(q => e.copy(child = q)))

          case n@Narrow(subtpe, child) => loop(path, subtpe, seen.withQuery(child)).map(_.map(q => n.copy(child = q)))
          case w@Wrap(_, child)        => loop(path, tpe, seen.withQuery(child)).map(_.map(q => w.copy(child = q)))
          case r@Rename(_, child)      => loop(path, tpe, seen.withQuery(child)).map(_.map(q => r.copy(child = q)))

          case g@Group(queries)        =>
            queries.foldM(seen.forGroup) {
              case (acc, q) => loop(path, tpe, acc.withQuery(q)).map(_.map(q => q :: acc.context))
            }.map(_.map(qs => g.copy(queries = qs.reverse)))

          case g@GroupList(queries)    =>
            queries.foldM(seen.forGroup) {
              case (acc, q) => loop(path, tpe, acc.withQuery(q)).map(_.map(q => q :: acc.context))
            }.map(_.map(qs => g.copy(queries = qs.reverse)))

          case u@Unique(_, child)      =>
            loop(path, tpe.nonNull, seen.withType(child, path, tpe)).map(_.map(q => u.copy(child = q)))

          case f@Filter(_, child)      =>
            loop(path, tpe.item, seen.withType(child, path, tpe)).map(_.map(q => f.copy(child = q)))

          case l@Limit(_, child)      =>
            loop(path, tpe.item, seen.withType(child, path, tpe)).map(_.map(q => l.copy(child = q)))

          case o@OrderBy(_, child)      =>
            loop(path, tpe.item, seen.withType(child, path, tpe)).map(_.map(q => o.copy(child = q)))

          case g@GroupBy(_, child)      =>
            loop(path, tpe.item, seen.withType(child, path, tpe)).map(_.map(q => g.copy(child = q)))

          case c@Query.Component(_, _, _) => seen.withQuery(c).rightIor
          case i: Introspect           => seen.withQuery(i).rightIor
          case d: Defer                => seen.withQuery(d).rightIor
          case Empty                   => seen.withQuery(Empty).rightIor

          case s: Skip                 => mkErrorResult(s"Unexpected Skip ${s.render}")
          case n: UntypedNarrow        => mkErrorResult(s"Unexpected UntypeNarrow ${n.render}")
          case Skipped                 => mkErrorResult(s"Unexpected Skipped")
        }
      }

      query match {
        case g@Group(queries) =>
          queries.traverse(q => loop(Nil, tpe, Seen(q))).map(eqs => g.copy(queries = eqs.map(_.context)))
        case g@GroupList(queries) =>
          queries.traverse(q => loop(Nil, tpe, Seen(q))).map(eqs => g.copy(queries = eqs.map(_.context)))
        case other =>
          loop(Nil, tpe, Seen(other)).map(_.context)
      }
    }
  }

  case class LeafCursor(path: List[String], tpe: Type, focus: Any, parent: Option[Cursor], env: Env) extends Cursor {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def mkChild(path: List[String] = path, tpe: Type = tpe, focus: Any = focus): LeafCursor =
      LeafCursor(path, tpe, focus, Some(this), Env.empty)

    def isLeaf: Boolean = tpe.isLeaf

    def asLeaf: Result[Json] =
      leafMapping[Any](tpe).map(_.encoder(focus).rightIor).getOrElse(
        focus match {
          case s: String => Json.fromString(s).rightIor
          case i: Int => Json.fromInt(i).rightIor
          case d: Double => Json.fromDouble(d) match {
              case Some(j) => j.rightIor
              case None => mkErrorResult(s"Unrepresentable double %d")
            }
          case b: Boolean => Json.fromBoolean(b).rightIor

          // This means we are looking at a column with no value because it's the result of a failed
          // outer join. This is an implementation error.
          case FailedJoin => sys.error("Unhandled failed join.")

          case other =>
            mkErrorResult(s"Not a leaf: $other")
        }
      )

    def isList: Boolean =
      tpe match {
        case ListType(_) => true
        case _ => false
      }

    def asList: Result[List[Cursor]] = (tpe, focus) match {
      case (ListType(tpe), it: List[_]) => it.map(f => mkChild(tpe = tpe, focus = f)).rightIor
      case _ => mkErrorResult(s"Expected List type, found $tpe")
    }

    def isNullable: Boolean =
      tpe match {
        case NullableType(_) => true
        case _ => false
      }

    def asNullable: Result[Option[Cursor]] =
      (tpe, focus) match {
        case (NullableType(_), None) => None.rightIor
        case (NullableType(tpe), Some(v)) => Some(mkChild(tpe = tpe, focus = v)).rightIor
        case _ => mkErrorResult("Not nullable")
      }

    def narrowsTo(subtpe: TypeRef): Boolean = false
    def narrow(subtpe: TypeRef): Result[Cursor] =
      mkErrorResult(s"Cannot narrow $tpe to $subtpe")

    def hasField(fieldName: String): Boolean = false
    def field(fieldName: String): Result[Cursor] =
      mkErrorResult(s"Cannot select field '$fieldName' from leaf type $tpe")

    def hasAttribute(attributeName: String): Boolean = false
    def attribute(attributeName: String): Result[Any] =
      mkErrorResult(s"Cannot read attribute '$attributeName' from leaf type $tpe")
  }

  case class SqlCursor(path: List[String], tpe: Type, focus: Any, mapped: MappedQuery, parent: Option[Cursor], env: Env) extends Cursor {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def mkChild(path: List[String] = path, tpe: Type = tpe, focus: Any = focus): SqlCursor =
      SqlCursor(path, tpe, focus, mapped, Some(this), Env.empty)

    def asTable: Result[Table] = focus match {
      case table@((_: Row) :: _ | Nil) => table.asInstanceOf[Table].rightIor
      case _ => mkErrorResult(s"Not a table")
    }

    def isUnstructured(tpe: Type): Boolean =
      tpe match {
        case NullableType(tpe) => isUnstructured(tpe)
        case ListType(tpe) => isUnstructured(tpe)
        case TypeRef(_, _) => tpe.dealias.isLeaf
        case _: ScalarType => true
        case _: EnumType => true
        case _ => false
      }

    def isLeaf: Boolean = false
    def asLeaf: Result[Json] =
      mkErrorResult(s"Not a leaf: $tpe")

    def isList: Boolean =
      tpe.isList

    def asList: Result[List[Cursor]] =
      if (!tpe.isList) mkErrorResult(s"Not a list: $tpe")
      else {
        val itemTpe = tpe.item.dealias
        asTable.map { table =>

          // The object mapping for `tpe`.
          val objectMapping0: ObjectMapping =
            objectMapping(path, itemTpe).getOrElse(sys.error(s"No ObjectMapping for $itemTpe"))

          // If this mapping is a list of child objects then its fields came from an outer join. If
          // there are no children then all keys defined in the mapping will have the `FailedJoin`
          // value.
          val isEmpty: Boolean =
            key(objectMapping0).forall { cr =>
              val ix = mapped.index(cr)
              table.forall(r => r(ix) == FailedJoin)
            }

          // Sanity check: isEmpty implies that we had zero rows, or one row with failed joins.
          if (isEmpty)
            assert(table.length <= 1)

          // Done!
          if (isEmpty) Nil
          else mapped.group(table, path, itemTpe).map(table => mkChild(tpe = itemTpe, focus = table))
        }
      }

    def isNullable: Boolean =
      tpe.isNullable

    def asNullable: Result[Option[Cursor]] =
      (tpe, focus) match {
        case (NullableType(_), Nil) => None.rightIor
        case (NullableType(tpe), _) => Some(mkChild(tpe = tpe)).rightIor // non-nullable column as nullable schema type (ok)
        case _ => mkErrorResult("Not nullable")
      }

    def narrowsTo(subtpe: TypeRef): Boolean = {
      val ctpe =
        objectMapping(path, tpe) match {
          case Some(im: SqlInterfaceMapping) =>
            im.discriminate(this).getOrElse(tpe)
          case Some(um: SqlUnionMapping) =>
            um.discriminate(this).getOrElse(tpe)
          case _ => tpe
        }
      if (ctpe =:= tpe)
        asTable.map(table => mapped.narrowsTo(table, path, subtpe)).right.getOrElse(false)
      else ctpe <:< subtpe
    }

    def narrow(subtpe: TypeRef): Result[Cursor] =
      if (narrowsTo(subtpe)) mkChild(tpe = subtpe).rightIor
      else mkErrorResult(s"Cannot narrow $tpe to $subtpe")

    def hasField(fieldName: String): Boolean =
      tpe.hasField(fieldName) && fieldMapping(path, tpe, fieldName).isDefined

    def field(fieldName: String): Result[Cursor] = {
      val fieldTpe = tpe.field(fieldName)
      fieldMapping(path, tpe.underlyingObject, fieldName) match {
        case Some(CursorField(_, f, _, _)) =>
          f(this).map(res => LeafCursor(fieldName :: path, fieldTpe, res, Some(this), Env.empty))

        case Some(SqlJson(_, _)) =>
          asTable.flatMap { table =>
            def mkCirceCursor(f: Json): Result[Cursor] =
              CirceCursor(path = fieldName :: path, tpe = fieldTpe, focus = f, parent = Some(this), env = Env.empty).rightIor
            mapped.selectField(table.head, path, tpe, fieldName).flatMap(_ match {
              case Some(j: Json) if fieldTpe.isNullable => mkCirceCursor(j)
              case None => mkCirceCursor(Json.Null)
              case j: Json if !fieldTpe.isNullable => mkCirceCursor(j)
              case other =>
                mkErrorResult(s"$fieldTpe: expected jsonb value found ${other.getClass}: $other")
            })
          }

        case _ =>
          if (isUnstructured(fieldTpe))
            asTable.flatMap(table =>
              mapped.selectField(table.head, path, tpe, fieldName).map { leaf =>
                val leafFocus = leaf match {
                  case Some(f) if tpe.variantField(fieldName) && !fieldTpe.isNullable => f
                  case other => other
                }
                LeafCursor(fieldName :: path, fieldTpe, leafFocus, Some(this), Env.empty)
              }
            )
          else {
            val fieldTpe = tpe.field(fieldName)
            asTable.map(table => mkChild(path = fieldName :: path, tpe = fieldTpe, focus = mapped.stripNulls(table, path, fieldTpe)))
          }
      }
    }

    def hasAttribute(attributeName: String): Boolean =
      fieldMapping(path, tpe, attributeName) match {
        case Some(CursorAttribute(_, _, _)) => true
        case _ => mapped.hasAttribute(path, tpe, attributeName)
      }

    def attribute(attributeName: String): Result[Any] =
      fieldMapping(path, tpe, attributeName) match {
        case Some(CursorAttribute(_, f, _)) => f(this)
        case _ =>
          asTable.flatMap(table => mapped.selectAttribute(table.head, path, tpe, attributeName))
      }
  }

  // overrides

  override def rootMapping(path: List[String], tpe: Type, fieldName: String): Option[RootMapping] =
    if (tpe =:= schema.queryType) super.rootMapping(path, tpe, fieldName)
    else Some(SqlRoot(fieldName, path, tpe))

  override def compilerPhases: List[QueryCompiler.Phase] =
    StagingElaborator :: super.compilerPhases

  override val interpreter: QueryInterpreter[F] =
    new QueryInterpreter(this) {

      override def runRootValues(queries: List[(Query, Type, Env)]): F[(Chain[Json], List[ProtoJson])] =
        for {
          _   <- monitor.stageStarted
          res <- runRootValues0(queries)
          _   <- monitor.stageCompleted
        } yield res

      override def runRootValue(query: Query, rootTpe: Type, env: Env): F[Result[ProtoJson]] =
        for {
          res <- super.runRootValue(query, rootTpe, env)
          _   <- monitor.resultComputed(res)
        } yield res

      def runRootValues0(queries: List[(Query, Type, Env)]): F[(Chain[Json], List[ProtoJson])] = {
        if (queries.length === 1)
          super.runRootValues(queries)
        else {
          def isGroupable(q: Query): Boolean =
            q match {
              case Context(_, Select(_, Nil, Filter(Eql(_, Const(_)), _))) => true
              case _ => false
            }

          def groupKey(q: (Query, Type, Env)): (List[String], String, Term[Any], Env, Query, Type) = {
            val (Context(cpath, Select(fieldName, Nil, Filter(Eql(path, Const(_)), child))), tpe, env) = q
            (cpath, fieldName, path, env, child, tpe)
          }

          def groupConst(q: Query): Eql[Any] = {
            val Context(_, Select(_, Nil, Filter(eql: Eql[Any] @unchecked, _))) = q
            eql
          }

          val deduped = queries.zipWithIndex.groupMap(_._1)(_._2)

          val (groupable, ungroupable) =
            deduped.partition(e => isGroupable(e._1._1))

          val grouped = groupable.groupMap {
            case (qt, _) => groupKey(qt)
          }{
            case ((q, _, _), is) => (groupConst(q), is)
          }

          val coalesced = grouped.map {
            case ((cpath, fieldName, _, env, child, tpe), cis) =>
              val ncis = (cis.map { case (q, is) => (q, is.sorted) }).toList.sortBy(_._2.head)
              val (eqls, is) = ncis.unzip
              val Some(in) = In.fromEqls[Any](eqls)
              ((Context(cpath, GroupBy(in.mkDiscriminator, Select(fieldName, Nil, Filter(in, child)))), ListType(tpe), env), is)
          }

          val ungroupableResults = {
            val (qts, is) = ungroupable.toList.unzip
            super.runRootValues(qts).map {
              case (errs, js) => (errs, js.zip(is))
            }
          }

          val coalescedResults = {
            val (qts, is) = coalesced.toList.unzip
            super.runRootValues(qts).map {
              case (errs, js) =>
                val unpacked = js.zip(is) flatMap { case (j, is) =>
                  ProtoJson.unpackList(j).getOrElse(is.map(_ => ProtoJson.fromJson(Json.Null))).zip(is)
                }

                (errs, unpacked)
            }
          }

          for {
            eurs <- ungroupableResults
            ecrs <- coalescedResults
          } yield {
            val (errs0, urs) = eurs
            val (errs1, crs) = ecrs
            val aligned = (urs.toList ++ crs.toList) flatMap { case (j, is) => is.map(i => (i, j)) }
            (errs0 ++ errs1, aligned.sortBy(_._1).map(_._2))
          }
        }
      }
    }

}
