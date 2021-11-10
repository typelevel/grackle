// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package sql

import scala.annotation.{nowarn, tailrec}

import cats.data.NonEmptyList
import cats.implicits._
import fs2.Stream
import io.circe.Json
import org.tpolecat.sourcepos.SourcePos

import Cursor.{Context, Env}
import Predicate._
import Query._
import QueryInterpreter.{mkErrorResult, mkOneError}
import circe.CirceMapping

/** An abstract mapping that is backed by a SQL database. */
trait SqlMapping[F[_]] extends CirceMapping[F] with SqlModule[F] { self =>

  override val validator: SqlMappingValidator =
    SqlMappingValidator(this)

  import AliasedMappings.FieldMappingType, FieldMappingType._
  import SqlQuery.{SqlJoin, SqlSelect}
  import TableExpr.{SubqueryRef, TableRef}

  /**
   * Name of a SQL schema column and its associated codec. Note that `Column`s are considered equal
   * if their table and column names are equal.
   */
  trait Column {
    def table: String
    def column: String
    def codec: Codec
    def scalaTypeName: String
    def pos: SourcePos

    def move(table: String, column: String = column): Column
    def subst(from: TableExpr, to: TableExpr): Column

    def isRef: Boolean = false

    override def equals(other: Any) =
      other match {
        case cr: Column => table == cr.table && column == cr.column
        case _ => false
      }

    override def hashCode(): Int =
      table.hashCode() + column.hashCode()

    /** This `Column` as a SQL expression of the form `table.column`. */
    def toFragment: Fragment
  }

  object Column {
    case class ColumnRef(table: String, column: String, codec: Codec, scalaTypeName: String, pos: SourcePos) extends Column {
      def move(table: String, column: String = column): Column =
        copy(table = table, column = column)

      def subst(from: TableExpr, to: TableExpr): Column =
        if (table == from.refName) copy(table = to.refName)
        else this

      override def isRef: Boolean = true

      def toFragment: Fragment = Fragments.const(s"$table.$column")
    }

    object ColumnRef {
      def apply(col: Column): ColumnRef =
        ColumnRef(col.table, col.column, col.codec, col.scalaTypeName, col.pos)
    }

    case class NullColumn(table: String, column: String, codec: Codec, scalaTypeName: String, pos: SourcePos) extends Column {
      def move(table: String, column: String = column): Column =
        copy(table = table, column = column)

      def subst(from: TableExpr, to: TableExpr): Column =
        if (table == from.refName) copy(table = to.refName)
        else this

      def toFragment: Fragment =
        Fragments.sqlTypeName(codec) match {
          case Some(name) => Fragments.const(s"(NULL :: $name)")
          case None => Fragments.const("NULL")
        }
    }

    object NullColumn {
      def apply(col: Column): NullColumn =
        NullColumn(col.table, col.column, col.codec, col.scalaTypeName, col.pos)
    }

    case class SubqueryColumn(table: String, column: String, codec: Codec, scalaTypeName: String, pos: SourcePos, subquery: SqlSelect) extends Column {
      def move(table: String, column: String = column): Column =
        copy(table = table, column = column)

      def subst(from: TableExpr, to: TableExpr): Column =
        if (table == from.refName) copy(table = to.refName)
        else this

      def toFragment: Fragment = Fragments.parentheses(subquery.toFragment) |+| Fragments.const(s" AS $column")
    }

    object SubqueryColumn {
      def apply(col: Column, subquery: SqlSelect): SubqueryColumn =
        SubqueryColumn(col.table, col.column, col.codec, col.scalaTypeName, col.pos, subquery)
    }

    case class CountColumn(table: String, column: String, codec: Codec, scalaTypeName: String, pos: SourcePos, cols: List[Column]) extends Column {
      def move(table: String, column: String = column): Column =
        copy(table = table, column = column)

      def subst(from: TableExpr, to: TableExpr): Column =
        if (table == from.refName) copy(table = to.refName)
        else this

      def toFragment: Fragment = Fragments.const("COUNT(DISTINCT(") |+| cols.map(_.toFragment).intercalate(Fragments.const(", ")) |+| Fragments.const(s")) AS $column")
    }

    object CountColumn {
      def apply(col: Column, cols: List[Column]): CountColumn =
        CountColumn(col.table, col.column, col.codec, col.scalaTypeName, col.pos, cols)
    }
  }

  /** A pair of `ColumnRef`s, representing a SQL join. */
  case class Join(parent: Column, child: Column)

  case class SqlRoot(fieldName: String, orootTpe: Option[Type] = None, mutation: Mutation = Mutation.None)(
    implicit val pos: SourcePos
  ) extends RootMapping {

    /**
      * Filters which can be compiled to SQL are eliminated here, partly to avoid duplicating
      * work programmatically, but also because the result set doesn't necessarily contain the
      * fields required for the filter predicates.
      */
    def stripFilters(query: Query, context: Context, am: AliasedMappings): Query = {
      def loop(query: Query, context: Context): Query =
        query match {
          case f@Filter(pred, _) if !am.isSqlTerm(context, pred) => f.copy(child = loop(f.child, context))
          case Filter(_, child) => loop(child, context)
          case PossiblyRenamedSelect(s@Select(fieldName, _, _), resultName) =>
            val fieldContext = context.forField(fieldName, resultName).getOrElse(sys.error(s"No field '$fieldName' of type ${context.tpe}"))
            PossiblyRenamedSelect(s.copy(child = loop(s.child, fieldContext)), resultName)
          case Rename(_, Count(_, _)) => Empty
          case Count(countName, _) => Select(countName, Nil, Empty)
          case Group(queries) => Group(queries.map(q => loop(q, context)))
          case GroupList(queries) => GroupList(queries.map(q => loop(q, context)))
          case u: Unique => u.copy(child = loop(u.child, context.asType(context.tpe.list)))
          case e: Environment => e.copy(child = loop(e.child, context))
          case w: Wrap => w.copy(child = loop(w.child, context))
          case r: Rename => r.copy(child = loop(r.child, context))
          case u: UntypedNarrow => u.copy(child = loop(u.child, context))
          case n@Narrow(subtpe, _) => n.copy(child = loop(n.child, context.asType(subtpe)))
          case s: Skip => s.copy(child = loop(s.child, context))
          case o: Offset => o.copy(child = loop(o.child, context))
          case l: Limit => l.copy(child = loop(l.child, context))
          case o: OrderBy => o.copy(child = loop(o.child, context))
          case other@(_: Component[_] | _: Defer | Empty | _: Introspect | _: Select | Skipped) => other
        }

      loop(query, context)
    }

    private def mkRootCursor(query: Query, context: Context, env: Env): F[Result[(Query, Cursor)]] = {
      (MappedQuery(query, context).map { mapped =>
        for {
          table <- mapped.fetch
          _     <- monitor.queryMapped(query, mapped.fragment, table)
        } yield {
          val stripped: Query = stripFilters(query, context, mapped.aliasedMappings)
          val cursor: Cursor = SqlCursor(context.asType(context.tpe.list), table, mapped, None, env)
          Result((stripped, cursor))
        }
      }).getOrElse(mkErrorResult("Unable to map query").widen.pure[F])
    }

    def cursor(query: Query, env: Env, resultName: Option[String]): Stream[F,Result[(Query, Cursor)]] =
      Stream.eval {
        (for {
          rootTpe  <- orootTpe
          fieldTpe <- rootTpe.underlyingField(fieldName)
        } yield {
          mkRootCursor(query, Context(fieldName, resultName, fieldTpe), env)
        }).getOrElse(mkErrorResult(s"Type ${orootTpe.getOrElse("unspecified type")} has no field '$fieldName'").pure[F])
      }

    def withParent(tpe: Type): SqlRoot =
      copy(orootTpe = Some(tpe))

  }

  sealed trait SqlFieldMapping extends FieldMapping {
    final def withParent(tpe: Type): FieldMapping = this
  }

  case class SqlField(
    fieldName: String,
    columnRef: Column.ColumnRef,
    key: Boolean = false,
    discriminator: Boolean = false,
    hidden: Boolean = false,
    associative: Boolean = false // a key which is also associative might occur multiple times in the table, ie. it is not a DB primary key
  )(implicit val pos: SourcePos) extends SqlFieldMapping

  case class SqlObject(fieldName: String, joins: List[Join])(
    implicit val pos: SourcePos
  ) extends SqlFieldMapping {
    final def hidden = false
  }
  object SqlObject {
    def apply(fieldName: String, joins: Join*): SqlObject = apply(fieldName, joins.toList)
  }

  case class SqlJson(fieldName: String, columnRef: Column.ColumnRef)(
    implicit val pos: SourcePos
  ) extends SqlFieldMapping {
    def hidden: Boolean = false
  }

  /**
   * Common super type for mappings which have a programmatic discriminator, ie. interface and union mappings.
   * TODO: support SQL-native discriminators.
   */
  sealed trait SqlDiscriminatedType {
    def discriminate(cursor: Cursor): Result[Type]
  }

  sealed trait SqlInterfaceMapping extends ObjectMapping with SqlDiscriminatedType

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

  sealed trait SqlUnionMapping extends ObjectMapping with SqlDiscriminatedType

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

  /**
   * Data structure managing DB table and column aliases.
   *
   * Aliases are scoped by the result path, which is unique in the GraphQL output
   * even in the presence of GraphQL field alises. Table aliases apply from the given
   * output path and below.
   *
   * Column aliases are synthesized whenever an SQL union has to be nested in a subquery
   * and hence requires a uniform set of column names. These column aliases apply from
   * the given path and above.
   *
   * Tables are only aliased if they have already been seen in an depth-first in-order
   * traversal of the output graph. This data structure also track the seen state for
   * tables.
   *
   * AliasedMappings also maintains the bookeeping information needed to supply
   * arbitrary fresh names. Currently these are used to name identity/predicate
   * joins.
   */
  case class AliasedMappings(tableAliases: List[(List[String], String, String)], columnAliases: List[(List[String], Column, Column)], seenTables: List[String], nextFresh: Int) {
    /** Creates a fresh name of the form <prefix>_<unique> */
    def fresh(prefix: String): (AliasedMappings, String) =
      (copy(nextFresh = nextFresh+1), s"${prefix}_${nextFresh}")

    /** Record that the given table has been seen and will require an alias if
     *  if referred to again.
     */
    def seenTable(table: String): AliasedMappings =
      copy(seenTables = (table :: seenTables).distinct)

    /** Recover the unaliased name of the given table */
    def unaliasTable(table: String): String =
      table.indexOf("_alias_") match {
        case -1 => table
        case n => table.take(n)
      }

    /** Recover the unaliased form of the given column */
    def unaliasColumn(col: Column): Column = {
      val baseTable = unaliasTable(col.table)
      val baseColumn =
        col.column.indexOf("_of_") match {
          case -1 => col.column
          case n => col.column.take(n)
        }
      col.move(table = baseTable, column = baseColumn)
    }

    /** Alias the given table at all the paths from the context result path and below */
    def aliasTable(context: Context, table: String): (AliasedMappings, String) = {
      val baseTable = unaliasTable(table)

      tableAliases.find(alias => alias._1 == context.resultPath && alias._2 == baseTable) match {
        case Some(existing) => (this, existing._3)
        case _ =>
          val alias = s"${baseTable}_alias_${tableAliases.size}"
          val aliases0 = (context.resultPath, baseTable, alias) :: tableAliases
          (copy(tableAliases = aliases0), alias)
      }
    }

    /** Derive a column alias for the given column in new table `table` */
    def computeColumnAlias(table: String, col: Column): Column =
      col.move(table = table, column = s"${col.column}_of_${col.table}")

    /** Derive column aliases for the given columns in new table `table` */
    def computeColumnAliases(table: String, cols: List[Column]): List[Column] =
      cols.map(col => col.move(table = table, column = s"${col.column}_of_${col.table}"))

    /** Alias the given columns to the suppiled aliased columns from the context
     *  result path and above
     */
    def aliasColumns(context: Context, cols: List[Column], aliasedCols: List[Column]): (AliasedMappings, List[Column]) = {
      val cols0 = cols.map(col => applyColumnAliases(context.resultPath, col))
      val aliases0 = (LazyList.continually(context.resultPath) lazyZip cols0 lazyZip aliasedCols).toList
      (copy(columnAliases = aliases0 ++ columnAliases), aliasedCols)
    }

    /** Returns the object mapping for `context` if any */
    def objectMapping(context: Context): Option[ObjectMapping] =
      context.tpe.underlyingObject.flatMap(obj => self.objectMapping(context.asType(obj)))

    /** Returns the field mapping for field `fieldName` in `context` if any */
    def fieldMapping(context: Context, fieldName: String): Option[FieldMapping] =
      objectMapping(context).flatMap(_.fieldMappings.find(_.fieldName == fieldName).orElse {
        context.tpe.underlyingObject match {
          case Some(ot: ObjectType) =>
            ot.interfaces.collectFirstSome(nt => fieldMapping(context.asType(nt), fieldName))
          case _ => None
        }
      })

    /** Returns the discriminator columns for the context type */
    def discriminatorColumnsForType(context: Context): List[Column] =
      objectMapping(context).map(_.fieldMappings.collect {
        case cm: SqlField if cm.discriminator => applyColumnAliases(context.resultPath, cm.columnRef)
      }).getOrElse(Nil)

    /** Returns the key columns for the context type */
    def keyColumnsForType(context: Context): List[Column] =
      objectMapping(context).map(_.fieldMappings.collect {
        case cm: SqlField if cm.key => applyColumnAliases(context.resultPath, cm.columnRef)
      }).getOrElse(Nil)

    /** Returns the columns for leaf field `fieldName` in `context` */
    def columnsForLeaf(context: Context, fieldName: String): List[Column] =
      fieldMapping(context, fieldName) match {
        case Some(SqlField(_, cr, _, _, _, _)) => List(applyColumnAliases(context.resultPath, cr))
        case Some(SqlJson(_, cr)) => List(applyColumnAliases(context.resultPath, cr))
        case Some(CursorFieldJson(_, _, _, required, _)) =>
          required.flatMap(r => columnsForLeaf(context, r))
        case Some(CursorField(_, _, _, required, _)) =>
          required.flatMap(r => columnsForLeaf(context, r))
        case None =>
          sys.error(s"No mapping for field '$fieldName' of type ${context.tpe}")
        case other =>
          sys.error(s"Non-leaf mapping for field '$fieldName' of type ${context.tpe}: $other")
      }

    /** Returns the aliased joins for constructing the subobject corresponding to the field `fieldName` in `context` */
    def joinsForSubObject(context: Context, fieldName: String, resultName: String, plural: Boolean): (AliasedMappings, List[SqlJoin]) =
      fieldMapping(context, fieldName).flatMap {
        case _: CursorFieldJson => Some((this, Nil))
        case SqlObject(_, Nil) => Some((this, Nil))
        case SqlObject(_, joins) =>
          val aliased = joins.map(j => this.applyJoinAliases(context.resultPath, j))
          for {
            fieldContext <- context.forField(fieldName, resultName)
            ct           <- this.parentTableForType(fieldContext)
          } yield {
            val inner = !fieldContext.tpe.isNullable && !fieldContext.tpe.isList
            val sjoins = SqlJoin.fromJoins(ct, aliased, plural, inner)
            val (am1, _, sjoins1) =
              sjoins.foldLeft((this, Option.empty[String], List.empty[SqlJoin])) {
                case ((am0, prevChild, sjoins0), sjoin0) =>
                  val sjoin = prevChild.map(c => sjoin0.substParent(TableRef.fromRefName(sjoin0.parent), TableRef.fromRefName(c))).getOrElse(sjoin0)
                  sjoin.child match {
                    case tr: TableRef =>
                      val (am, join) =
                        if(am0.seenTables.contains(tr.refName)) {
                          val aliased = am0.aliasTable(fieldContext, tr.refName)
                          (aliased._1, sjoin.substChild(tr, tr.copy(alias = Some(aliased._2))))
                        } else
                          (am0.seenTable(tr.refName), sjoin)

                      if (isAssociative(fieldContext)) {
                        // TODO: Merge this with the filter/order by/limit predicate join logic
                        val (am1, idTable) = am.fresh("identity")
                        val keys = am.keyColumnsForType(fieldContext)
                        val join0 = join.subst(join.child, TableRef(tr.baseName, Some(idTable)))
                        val joinChild = join.child.refName
                        val on = keys.map(key => (key.move(table = idTable), key.move(table = joinChild)))
                        val selfJoin = SqlJoin(idTable, join.child, on, plural, false, false)
                        (am1, Some(selfJoin.child.refName), selfJoin :: join0 :: sjoins0)
                      } else {
                        (am, Some(join.child.refName), join :: sjoins0)
                      }
                    case _: SubqueryRef =>
                      (am0, Some(sjoin.child.refName), sjoin :: sjoins0)
                  }
              }

            (am1, sjoins1.reverse)
          }
        case _ => None
      }.getOrElse(
        sys.error(s"Non-subobject mapping for field '$fieldName' of type ${context.tpe}")
      )

    /** Returns the aliased joins for traversing the path `suffix` from the context path */
    def joinsForPath(context: Context, suffix: List[String]): (AliasedMappings, List[SqlJoin]) =
      suffix match {
        case Nil => (this, Nil)
        case _ :: Nil => (this, Nil)
        case hd :: tl =>
          joinsForSubObject(context, hd, hd, false) match {
            case (am, joins) =>
              (for {
                pt           <- parentTableForType(context)
                fieldContext <- context.forField(hd, hd)
              } yield {
                val (am0, joins0) = am.copy(seenTables = pt.refName :: am.seenTables).joinsForPath(fieldContext, tl)
                (am0, joins ++ joins0)
              }).getOrElse((am, joins))
          }
      }

    /** Returns the aliased columns corresponding to `term` in `context` */
    def columnForSqlTerm[T](context: Context, term: Term[T]): Option[Column] =
      term match {
        case termPath: Path =>
          context.forPath(termPath.path.init).flatMap { parentContext =>
            columnForAtomicField(parentContext, termPath.path.last)
          }
        case _ => None
      }

    /** Returns the aliased column corresponding to the atomic field `fieldName` in `context` */
    def columnForAtomicField(context: Context, fieldName: String): Option[Column] = {
      fieldMapping(context, fieldName) match {
        case Some(SqlField(_, cr, _, _, _, _)) => Some(applyColumnAliases(context.resultPath, cr))
        case Some(SqlJson(_, cr)) => Some(applyColumnAliases(context.resultPath, cr))
        case _ => None
      }
    }

    /** Returns the aliased columns corresponding to `term` in `context` */
    def columnsForTerm(context: Context, term: Term[_]): List[Column] =
      (term.fold(List.empty[Column]) {
        case (acc, termPath: Path) =>
          context.forPath(termPath.path.init).map { parentContext =>
            columnsForLeaf(parentContext, termPath.path.last) ++ acc
          }.getOrElse(acc)
        case (acc, _) => acc
      }).distinct

    /** Returns the `Encoder` for the given type */
    def encoderForLeaf(tpe: Type): Option[io.circe.Encoder[Any]] =
      leafMapping[Any](tpe).map(_.encoder)

    /** Returns the `Encoder` for the given term in `context` */
    def encoderForTerm(context: Context, term: Term[_]): Option[Encoder] =
      term match {
        case pathTerm: Path =>
          for {
            cr <- columnForSqlTerm(context, pathTerm) // encoder is independent of query aliases
          } yield toEncoder(cr.codec)

        case (_: And)|(_: Or)|(_: Not)|(_: Eql[_])|(_: NEql[_])|(_: Lt[_])|(_: LtEql[_])|(_: Gt[_])|(_: GtEql[_])  => Some(booleanEncoder)
        case (_: AndB)|(_: OrB)|(_: XorB)|(_: NotB) => Some(intEncoder)
        case (_: ToUpperCase)|(_: ToLowerCase) => Some(stringEncoder)
        case _ => None
      }

    /** Returns the discriminator for the type at `context` */
    def discriminatorForType(context: Context): Option[SqlDiscriminatedType] =
      objectMapping(context) collect {
        //case d: SqlDiscriminatedType => d  // Fails in 2.13.6 due to https://github.com/scala/bug/issues/12398
        case i: SqlInterfaceMapping => i
        case u: SqlUnionMapping => u
      }

    /** Returns the fully aliased table for the type at `context` */
    def parentTableForType(context: Context): Option[TableRef] =
      for {
        om  <- objectMapping(context)
        cr  <- om.fieldMappings.collectFirst {
                 case SqlField(_, cr, _, _, _, _) => applyColumnAliases(context.resultPath, cr)
               }
      } yield TableRef.fromRefName(cr.table)

    /** Return an indicator of the kind of field mapping correspoding to `fieldName` in `context` */
    def fieldMappingType(context: Context, fieldName: String): Option[FieldMappingType] =
      fieldMapping(context, fieldName).flatMap {
        case CursorField(_, f, _, _, _) => Some(CursorFieldMapping(f))
        case CursorFieldJson(_, f, _, _, _) => Some(CursorFieldJsonMapping(f))
        case _: SqlJson => Some(JsonFieldMapping)
        case _: SqlField => Some(LeafFieldMapping)
        case _: SqlObject => Some(ObjectFieldMapping)
        case _ => None
      }

    /** Is `fieldName` in `context` Jsonb? */
    def isJsonb(context: Context, fieldName: String): Boolean =
      fieldMapping(context, fieldName) match {
        case Some(_: SqlJson) => true
        case Some(_: CursorFieldJson) => true
        case _ => false
      }

    /** Is `fieldName` in `context` computed? */
    def isComputedField(context: Context, fieldName: String): Boolean =
      fieldMapping(context, fieldName) match {
        case Some(_: CursorField[_]) => true
        case _ => false
      }

    /** Is `term` in `context`expressible in SQL? */
    def isSqlTerm(context: Context, term: Term[_]): Boolean =
      term.forall {
        case termPath: Path =>
          context.forPath(termPath.path.init).map { parentContext =>
            !isComputedField(parentContext, termPath.path.last)
          }.getOrElse(true)
        case _: Const[_] | _: And | _: Or | _: Not | _: Eql[_] | _: NEql[_] | _: Contains[_] | _: Lt[_] | _: LtEql[_] | _: Gt[_] |
             _: GtEql[_] | _: In[_] | _: AndB | _: OrB | _: XorB | _: NotB | _: Matches | _: StartsWith | _: IsNull[_] |
             _: ToUpperCase | _: ToLowerCase | _: Like => true
        case _ => false
      }

    /** Is the context type mapped to an associative table? */
    def isAssociative(context: Context): Boolean =
      objectMapping(context).map(_.fieldMappings.exists {
        case sf: SqlField => sf.associative
        case _ => false
      }).getOrElse(false)

    /** Does the type of `fieldName` in `context` represent a list of subobjects? */
    def nonLeafList(context: Context, fieldName: String): Boolean =
      context.tpe.underlyingField(fieldName).map { fieldTpe =>
        fieldTpe.nonNull.isList && (
          fieldMapping(context, fieldName).map {
            case SqlObject(_, joins) => joins.nonEmpty
            case _ => false
          }.getOrElse(false)
        )
      }.getOrElse(false)

    /** Does the result of `query` in `context` contain lists of subobjects? */
    def containsNonLeafList(query: Query, context: Context): Boolean = {
      def loop(query: Query, context: Context, maybe: Boolean): Boolean = {
        query match {
          case Select(fieldName, _, child) =>
            maybe || context.forField(fieldName, fieldName).map { fieldContext =>
              loop(child, fieldContext, nonLeafList(context, fieldName))
            }.getOrElse(false)

          case Group(queries)        => queries.exists(q => loop(q, context, maybe))
          case GroupList(queries)    => queries.exists(q => loop(q, context, maybe))

          case Filter(_, child)      => loop(child, context, maybe)
          case Unique(child)         => loop(child, context, false)
          case Limit(n, child)       => loop(child, context, maybe && n > 1)
          case Offset(_, child)      => loop(child, context, maybe)
          case OrderBy(_, child)     => loop(child, context, maybe)

          case Narrow(_, child)      => loop(child, context, maybe)
          case Wrap(_, child)        => loop(child, context, maybe)
          case Rename(_, child)      => loop(child, context, maybe)
          case _: Count              => maybe

          case Environment(_, child) => loop(child, context, maybe)

          case Query.Component(_, _, _) => maybe
          case _: Introspect         => maybe
          case _: Defer              => maybe

          case _: Skip               => maybe
          case _: UntypedNarrow      => maybe
          case Skipped               => maybe
          case Empty                 => maybe
        }
      }

      loop(query, context, false)
    }

    /** Return the fully aliased column corresponding to the given unaliased columne `cr` in `context` */
    private def applyColumnAliases(resultPath: List[String], cr: Column): Column = {
      // First apply any in-scope table aliases
      val colWithTableAliases =
        tableAliases.filter(alias => resultPath.endsWith(alias._1) && alias._2 == cr.table).sortBy(-_._1.length)
          .headOption.map {
            case (_, _, alias) => cr.move(table = alias)
          }.getOrElse(cr)

      // Apply any column aliases
      val aliased =
        columnAliases.filter(alias => resultPath.endsWith(alias._1) && alias._2 == colWithTableAliases).sortBy(_._1.length)
          .headOption.map(_._3).getOrElse(colWithTableAliases)

      if (aliased == cr) aliased
      else applyColumnAliases(resultPath, aliased)
    }

    /** Return the given join with any column aliases applied */
    private def applyJoinAliases(resultPath: List[String], join: Join): Join =
      Join(applyColumnAliases(resultPath, join.parent), applyColumnAliases(resultPath, join.child))
  }

  object AliasedMappings {
    def empty: AliasedMappings = AliasedMappings(Nil, Nil, Nil, 0)

    /** Enumeration representing a kind of field mapping */
    sealed trait FieldMappingType
    object FieldMappingType {
      /** Field mapping is a subobject */
      case object ObjectFieldMapping extends FieldMappingType
      /** Field mapping is a leaf */
      case object LeafFieldMapping extends FieldMappingType
      /** Field mapping is a Json subobject */
      case object JsonFieldMapping extends FieldMappingType
      /** Field mapping is computed */
      case class CursorFieldMapping(f: Cursor => Result[Any]) extends FieldMappingType
      case class CursorFieldJsonMapping(f: Cursor => Result[Json]) extends FieldMappingType
    }
  }

  /** Representation of a table expression */
  sealed trait TableExpr {
    /** The underlying name of the table. This will be `None` if the table expression is a subquery */
    def name: Option[String]
    /** Any alias applied to the table expression. Always present for subqueries */
    def alias: Option[String]
    /** The table name used to refer to this table */
    def refName: String

    def toFragment: Fragment
  }

  object TableExpr {
    /** Table expression corresponding to a possibly aliased table */
    case class TableRef(baseName: String, alias: Option[String] = None) extends TableExpr {
      def name: Option[String] = Some(baseName)
      def refName: String = alias.getOrElse(baseName)
      def toFragment: Fragment = Fragments.const(alias.map(a => s"$baseName AS $a").getOrElse(baseName))
    }

    object TableRef {
      def fromRefName(refName: String): TableRef =
        refName.indexOf("_alias_") match {
          case -1 => TableRef(refName)
          case n => TableRef(refName.take(n), Some(refName))
        }
    }

    /** Table expression corresponding to a subquery */
    case class SubqueryRef(subquery: SqlQuery, alias0: String) extends TableExpr {
      def name: Option[String] = None
      def alias: Option[String] = Some(alias0)
      def refName = alias0
      def toFragment: Fragment = Fragments.parentheses(subquery.toFragment) |+| Fragments.const(s"AS $alias0")
    }
  }

  /** Representation of a SQL query in a context */
  sealed trait SqlQuery {
    /** The context for this query */
    def context: Context

    /** This query in the given context */
    def withContext(context: Context): SqlQuery

    /** Execute this query in `F` */
    def fetch: F[Table]

    /** The index of the given unaliased column in the result set */
    def index(col: Column): Int

    /** Add the given columns to this query */
    def addColumns(cols: List[Column]): SqlQuery

    /** Nest this query as a subobject in the enclosing `parentContext` */
    def nest(
      parentContext: Context,
      fieldName: String,
      resultName: String,
      extraCols: List[Column],
      extraJoins: List[SqlJoin],
      aliasedMappings: AliasedMappings
    ): (SqlQuery, AliasedMappings)

    /** Add WHERE, ORDER BY, OFFSET and LIMIT to this query */
    def addFilterOrderByLimit(
      pred: Option[Predicate],
      oss: Option[List[OrderSelection[_]]],
      offset: Option[Int],
      limit: Option[Int],
      folQuery: Option[SqlSelect],
      aliasedMappings: AliasedMappings
    ): Option[(SqlQuery, AliasedMappings)]

    def toFragment: Fragment
  }

  object SqlQuery {
    /** Combine the given queries as a single SQL query */
    def combineAll(queries: List[SqlQuery]): Option[SqlQuery] = {
      val (selects, unions) =
        queries.partitionMap {
          case s: SqlSelect => Left(s)
          case u: SqlUnion => Right(u)
        }

      def combineSelects(sels: List[SqlSelect]): List[SqlSelect] = {
        if (sels.sizeCompare(1) == 0) sels
        else {
          val (context, table, orders, offset, limit, distinct) = {
            val first = sels.head
            (first.context, first.table, first.orders, first.offset, first.limit, first.distinct)
          }
          val (noJoins, joins) = sels.partition(_.joins.isEmpty)

          def combineSameJoins(sels: List[SqlSelect]): SqlSelect = {
            val cols = sels.flatMap(_.cols).distinct
            val wheres = sels.flatMap(_.wheres).distinct
            SqlSelect(context, table, cols, sels.head.joins, wheres, orders, offset, limit, distinct)
          }
          val sameJoins = joins.groupBy(_.joins).values.map(combineSameJoins).toList

          val (pluralJoins, singularJoins) = sameJoins.partition(_.plural)

          val combined: Option[SqlSelect] = {
            val cols = (singularJoins.flatMap(_.cols) ++ noJoins.flatMap(_.cols)).distinct
            val joins = singularJoins.flatMap(_.joins)
            val wheres = (singularJoins.flatMap(_.wheres) ++ noJoins.flatMap(_.wheres)).distinct
            if (cols.isEmpty) {
              assert(joins.isEmpty && wheres.isEmpty)
              None
            } else Some(SqlSelect(context, table, cols, joins, wheres, orders, offset, limit, distinct))
          }

          (combined, pluralJoins) match {
            case (None, p) => p
            case (Some(c), Nil) => List(c)
            case (Some(c), p :: ps) =>
              val cols = (c.cols ++ p.cols).distinct
              val joins = (c.joins ++ p.joins)
              val wheres = (c.wheres ++ p.wheres).distinct
              p.copy(cols = cols, joins = joins, wheres = wheres) :: ps
          }
        }
      }

      val combinedSelects = selects.groupBy(sel => (sel.context, sel.table, sel.orders, sel.limit, sel.distinct)).values.flatMap(combineSelects).toList
      if (combinedSelects.sizeCompare(1) == 0 && unions.isEmpty) Some(combinedSelects.head)
      else {
        val unionSelects = unions.flatMap(_.elems)
        val allSelects = combinedSelects ++ unionSelects
        Some(SqlUnion(allSelects))
      }
    }

    /** Construct a WHERE clause for the given predicate `pred` in `context` */
    def mkWhere(context: Context, pred: Predicate, aliasedMappings: AliasedMappings): Option[Where] = {
      import aliasedMappings._

      def unify(x: Term[_], y: Term[_]): Option[Encoder] =
        encoderForTerm(context, x).orElse(encoderForTerm(context, y))

      def loop(term: Term[_], e: Encoder): Option[Where] = {

        def unaryOp(x: Term[_])(op: Fragment, enc: Option[Encoder]): Option[Where] =
          for {
            e  <- enc.orElse(encoderForTerm(context, x))
            nx <- loop(x, e)
          } yield {
            val frag = op |+| nx.toFragment
            Where(frag)
          }

        def binaryOp(x: Term[_], y: Term[_])(op: Fragment, enc: Option[Encoder] = None): Option[Where] =
          for {
            e  <- enc.orElse(unify(x, y))
            nx <- loop(x, e)
            ny <- loop(y, e)
          } yield {
            val frag = nx.toFragment |+| op |+| ny.toFragment
            Where(frag)
          }

        def binaryOp2(x: Term[_])(op: Where => Fragment, enc: Option[Encoder] = None): Option[Where] =
          for {
            e  <- enc.orElse(encoderForTerm(context, x))
            nx <- loop(x, e)
          } yield {
            val frag = op(nx)
            Where(frag)
          }

        term match {
          case Const(value) =>
            Some(Where(Fragments.bind(e, value)))

          case pathTerm: Path =>
            aliasedMappings.columnForSqlTerm(context, pathTerm).map { col => Where(col.toFragment) }

          case And(x, y) =>
            binaryOp(x, y)(Fragments.const(" AND "), Some(booleanEncoder))

          case Or(x, y) =>
            binaryOp(x, y)(Fragments.const(" OR "), Some(booleanEncoder))

          case Not(x) =>
            unaryOp(x)(Fragments.const(" NOT "), Some(booleanEncoder))

          case Eql(x, y) =>
            binaryOp(x, y)(Fragments.const(" = "))

          case Contains(x, y) =>
            binaryOp(x, y)(Fragments.const(" = "))

          case NEql(x, y) =>
            binaryOp(x, y)(Fragments.const(" != "))

          case Lt(x, y) =>
            binaryOp(x, y)(Fragments.const(" < "))

          case LtEql(x, y) =>
            binaryOp(x, y)(Fragments.const(" <= "))

          case Gt(x, y) =>
            binaryOp(x, y)(Fragments.const(" > "))

          case GtEql(x, y) =>
            binaryOp(x, y)(Fragments.const(" >= "))

          case In(x, y) =>
            for {
              enc <- encoderForTerm(context, x)
              ys  <- NonEmptyList.fromList(y)
              w   <- binaryOp2(x)(nx => Fragments.in(nx.toFragment, ys, enc))
            } yield w

          case AndB(x, y) =>
            binaryOp(x, y)(Fragments.const(" & "), Some(intEncoder))

          case OrB(x, y) =>
            binaryOp(x, y)(Fragments.const(" | "), Some(intEncoder))

          case XorB(x, y) =>
            binaryOp(x, y)(Fragments.const(" # "), Some(intEncoder))

          case NotB(x) =>
            unaryOp(x)(Fragments.const(" NOT "), Some(intEncoder))

          case Matches(x, regex) =>
            binaryOp2(x)(nx =>
              Fragments.const("regexp_matches(") |+|
                nx.toFragment |+| Fragments.const(s", ") |+|
                Fragments.bind(stringEncoder, regex.toString) |+|
              Fragments.const(s")"),
              Some(stringEncoder)
            )

          case StartsWith(x, prefix) =>
            binaryOp2(x)(nx => nx.toFragment |+| Fragments.const(s" LIKE ") |+| Fragments.bind(stringEncoder, prefix + "%"), Some(stringEncoder))

          case ToUpperCase(x) =>
            binaryOp2(x)(nx => Fragments.const("upper(") |+| nx.toFragment |+| Fragments.const(s")"), Some(stringEncoder))

          case ToLowerCase(x) =>
            binaryOp2(x)(nx => Fragments.const("lower(") |+| nx.toFragment |+| Fragments.const(s")"), Some(stringEncoder))

          case IsNull(x, isNull) =>
            val sense = if (isNull) "" else "NOT"
            binaryOp2(x)(nx => nx.toFragment |+| Fragments.const(s" IS $sense NULL "))

          case Like(x, pattern, caseInsensitive) =>
            val op = if(caseInsensitive) " ILIKE " else " LIKE "
            binaryOp2(x)(nx => nx.toFragment |+| Fragments.const(s" $op ") |+| Fragments.bind(stringEncoder, pattern), Some(stringEncoder))

          case _ =>
            None
        }
      }

      if(aliasedMappings.isSqlTerm(context, pred)) loop(pred, booleanEncoder)
      else None
    }

    /** Compute the set of paths traversed by the given prediate */
    def wherePaths(pred: Predicate): List[List[String]] = {
      def loop(term: Term[_], acc: List[List[String]]): List[List[String]] = {
        term match {
          case pathTerm: Path   => pathTerm.path :: acc
          case And(x, y)        => loop(y, loop(x, acc))
          case Or(x, y)         => loop(y, loop(x, acc))
          case Not(x)           => loop(x, acc)
          case Eql(x, y)        => loop(y, loop(x, acc))
          case Contains(x, y)   => loop(y, loop(x, acc))
          case NEql(x, y)       => loop(y, loop(x, acc))
          case Lt(x, y)         => loop(y, loop(x, acc))
          case LtEql(x, y)      => loop(y, loop(x, acc))
          case Gt(x, y)         => loop(y, loop(x, acc))
          case GtEql(x, y)      => loop(y, loop(x, acc))
          case In(x, _)         => loop(x, acc)
          case AndB(x, y)       => loop(y, loop(x, acc))
          case OrB(x, y)        => loop(y, loop(x, acc))
          case XorB(x, y)       => loop(y, loop(x, acc))
          case NotB(x)          => loop(x, acc)
          case Matches(x, _)    => loop(x, acc)
          case StartsWith(x, _) => loop(x, acc)
          case ToUpperCase(x)   => loop(x, acc)
          case ToLowerCase(x)   => loop(x, acc)
          case Like(x, _, _)    => loop(x, acc)
          case _                => acc
        }
      }

      loop(pred, Nil)
    }

    /** Construct an `SQLSelect` corresponding to the given orderings applied to `table` in `context` */
    def mkOrderBy(
      context: Context,
      table: TableExpr,
      oss: List[OrderSelection[_]],
      aliasedMappings: AliasedMappings
    ): Option[SqlSelect] = {
      oss.map { os =>
        val OrderSelection(term, ascending, nullsLast) = os
        term match {
          case pathTerm: Path =>
            aliasedMappings.columnForSqlTerm(context, pathTerm).map { col =>
              val dir = if(ascending) "" else " DESC"
              val nulls = s" NULLS ${if(nullsLast) "LAST" else "FIRST"}"
              val collatedCol =
                if (!Fragments.needsCollation(col.codec)) col.toFragment
                else Fragments.parentheses(col.toFragment |+| Fragments.const(s""" COLLATE "C""""))
              val frag = collatedCol |+| Fragments.const(s"$dir$nulls")
              (col, Order(frag))
            }
          case other =>
            sys.error(s"Expected path term for ordering but found $other")
        }
      }.sequence.map { fragsAndCols =>
        val (cols, frags) = fragsAndCols.unzip
        SqlSelect(context, table, cols, Nil, Nil, frags, None, None, false, Nil, cols.filter(col => Fragments.needsCollation(col.codec)).toSet)
      }
    }

    /** Representation of an SQL SELECT */
    case class SqlSelect(
      context:  Context,             // the GraphQL context of the query
      table:    TableExpr,           // the table/subquery
      cols:     List[Column],        // the requested columns
      joins:    List[SqlJoin],       // joins for predicates/subobjects
      wheres:   List[Where],
      orders:   List[Order],
      offset:   Option[Int],
      limit:    Option[Int],
      distinct: Boolean,             // DISTINCT or not
      aliases:  List[String] = Nil,  // column aliases if any
      collate:  Set[Column] = Set.empty[Column]  // The set of columns requiring collation
    ) extends SqlQuery {
      assert(aliases.isEmpty || aliases.sizeCompare(cols) == 0)
      assert(SqlJoin.checkOrdering(table.refName, joins))

      /** This query in the given context */
      def withContext(context: Context): SqlSelect =
        copy(context = context)

      /** Execute this query in `F` */
      def fetch: F[Table] =
        self.fetch(toFragment, cols.map(col => (col.table != table.refName, col.codec)))

      /** The index of the given unaliased column in the result set */
      def index(col: Column): Int = {
        val i = cols.indexOf(col)
        if (i < 0) throw new RuntimeException(s"Unmapped column ${col.column} of table ${col.table}")
        i
      }

      /** Add the given columns to this query */
      def addColumns(extraCols: List[Column]): SqlSelect = {
        val cols0 = (cols ++ extraCols).distinct

        table match {
          case _: TableRef =>
            copy(cols = cols0)
          case sr@SubqueryRef(sq, _) =>
            val extraCols0 = extraCols.filter(_.table == table.refName)
            val extraCols1 =
              sq match {
                case s: SqlSelect => extraCols0.map(_.move(table = s.table.refName))
                case _: SqlUnion => extraCols0
              }
            copy(cols = cols0, table = sr.copy(subquery = sq.addColumns(extraCols1)))
        }
      }

      /** Nest this query as a subobject in the enclosing `parentContext` */
      def nest(
        parentContext: Context,
        fieldName: String,
        resultName: String,
        extraCols: List[Column],
        extraJoins: List[SqlJoin],
        aliasedMappings: AliasedMappings
      ): (SqlSelect, AliasedMappings) = {
        if (extraJoins.isEmpty)
          (copy(context = parentContext, cols = (extraCols ++ cols).distinct), aliasedMappings)
        else {
          val parentTable: TableRef =
            aliasedMappings.parentTableForType(parentContext).
              getOrElse(sys.error(s"No parent table for type ${parentContext.tpe}"))

          val newJoins =
            table match {
              case sr@SubqueryRef(sq: SqlSelect, _) =>
                // If this SELECT is on a SELECT subquery then push the columns required for the
                // joins down into the subquery
                val sqCols = (extraJoins.flatMap(_.colsOfChild(table)).map(_.move(table = sq.table.refName)) ++ sq.cols).distinct

                extraJoins.map { ij =>
                  sq match {
                    // If the subquery is an idenity/predicate join then push the join
                    // conditions into it as additional where clauses
                    case SqlSelect(_, _, _,
                      List(
                        psj@SqlJoin(_,
                          psr@SubqueryRef(pred: SqlSelect, predName),
                          _, _, _, _
                        )
                      ),
                      _, _, _, _, _, _, _) if predName.startsWith("pred_") =>

                      val wheres = ij.on.map {
                        case (p, c) =>
                          Where(c.move(table = sq.table.refName).toFragment |+| Fragments.const(" = ") |+| p.toFragment)
                      }

                      val sq0 =
                        sq.copy(
                          cols = sqCols,
                          joins = List(psj.copy(child = psr.copy(subquery = pred.copy(wheres = wheres ++ pred.wheres))))
                        )
                      val sr0 = sr.copy(subquery = sq0)
                      ij.subst(table, sr0).copy(lateral = true)

                    case _ =>
                      val sq0 = sq.copy(cols = sqCols)
                      val sr0 = sr.copy(subquery = sq0)
                      ij.subst(table, sr0)
                  }
                }

              case _ =>
                extraJoins
            }

          (copy(context = parentContext, table = parentTable, cols = (cols ++ extraCols).distinct, joins = (newJoins ++ joins.filterNot(newJoins.contains))), aliasedMappings)
        }
      }

      /** Add WHERE, ORDER BY and LIMIT to this query */
      def addFilterOrderByLimit(
        pred: Option[Predicate],
        oss: Option[List[OrderSelection[_]]],
        offset: Option[Int],
        limit: Option[Int],
        folQuery: Option[SqlSelect],
        aliasedMappings: AliasedMappings
      ): Option[(SqlSelect, AliasedMappings)] = {
        def combine[T, U](ot: Option[T], u: U, f: (T, U) => Option[U]): Option[U] =
          ot match {
            case None => Some(u)
            case Some(t) => f(t, u)
          }

        (pred, oss, limit) match {
          case (Some(pred), _, _) if !aliasedMappings.isSqlTerm(context, pred) =>
            Some((this, aliasedMappings))
          case _ =>
            val predMappings = AliasedMappings.empty

            val keyCols = predMappings.keyColumnsForType(context)

            // GraphQL-level keys aren't necessarily DB-level keys, so could be null. Exclude these.
            val nonNullKeys = keyCols.map(col => Where(col.toFragment |+| Fragments.const(" IS NOT NULL")))

            // If there is a limit then use a fallback order for determinism
            val keyOrder = limit.map(_ => keyCols.map(col => Order(col.toFragment |+| Fragments.const(" ASC")))).getOrElse(Nil)

            def doInit(folQuery: SqlSelect, @nowarn sel: SqlSelect): Option[SqlSelect] =
              Some(folQuery.copy(cols = keyCols, orders = keyOrder, distinct = true))

            def doWhere(pred: Predicate, sel: SqlSelect): Option[SqlSelect] =
              mkWhere(context, pred, predMappings).map(where => sel.copy(wheres = List(where)))

            def doOrderBy(oss: List[OrderSelection[_]], sel: SqlSelect): Option[SqlSelect] =
              mkOrderBy(context, table, oss, predMappings).map { os =>
                // Ensure the fallback order only include cols we aren't already ordering on
                val keyOrder = keyCols.diff(os.cols).map(col => Order(col.toFragment |+| Fragments.const(" ASC")))
                sel.copy(cols = (os.cols ++ sel.cols).distinct, orders = os.orders ++ keyOrder, collate = sel.collate ++ os.collate)
              }

            def doOffset(off: Int, sel: SqlSelect): Option[SqlSelect] =
              Some(sel.copy(offset = Some(off)))

            def doLimit(lim: Int, sel: SqlSelect): Option[SqlSelect] =
              Some(sel.copy(limit = Some(lim)))

            val predTable = table match {
              case tr: TableRef => TableRef(predMappings.unaliasTable(tr.baseName))
              case sr: SubqueryRef => sr
            }

            for {
              withKeys    <- combine(folQuery, SqlSelect(context, predTable, keyCols, Nil, Nil, keyOrder, None, None, true), doInit)
              withWhere   <- combine(pred, withKeys, doWhere)
              withOrderBy <- combine(oss, withWhere, doOrderBy)
              modifier0   <- combine(offset, withOrderBy, doOffset)
              modifier1   <- combine(limit, modifier0, doLimit)
              modifier    =  modifier1.copy(wheres = nonNullKeys ++ modifier1.wheres)
            } yield {
              val (am, alias) = aliasedMappings.fresh("pred")
              val on = keyCols.map(key => (key.move(table = predTable.refName), key.move(table = alias)))
              val predJoin = SqlJoin(predTable.refName, SubqueryRef(modifier, alias), on, false, false, true)
              val selAll = SqlSelect(context, predTable, colsOf(table).filter(_.isRef).map(_.move(table = predTable.refName)), List(predJoin), Nil, Nil, None, None, false)
              val subRef = SubqueryRef(selAll, table.refName)
              (copy(table = subRef), am)
            }
        }
      }

      def toFragment: Fragment = {
        val dist = if (distinct) " DISTINCT" else ""

        def collated(col: Column): Fragment =
          if (collate.contains(col)) Fragments.parentheses(col.toFragment |+| Fragments.const(""" COLLATE "C"""")) else col.toFragment

        val cols0 =
          if (aliases.isEmpty) cols.map(collated)
          else cols.zip(aliases).map { case (c, a) => collated(c) |+| Fragments.const(s" AS $a") }

        val select = Fragments.const(s"SELECT$dist ") |+| cols0.intercalate(Fragments.const(", "))

        val from = Fragments.const(" FROM ") |+| table.toFragment

        val joins0 = joins.map(_.toFragment).combineAll

        val where = Fragments.const(" ") |+| Fragments.whereAnd(wheres.map(_.toFragment): _*)

        val orderBy =
          if (orders.isEmpty) Fragments.empty
          else Fragments.const(" ORDER BY ") |+| orders.map(_.toFragment).intercalate(Fragments.const(","))

        val off = offset.map(o => Fragments.const(s" OFFSET $o")).getOrElse(Fragments.empty)

        val lim = limit.map(l => Fragments.const(s" LIMIT $l")).getOrElse(Fragments.empty)

        select |+| from |+| joins0 |+| where |+| orderBy |+| off |+| lim
      }

      /** The columns of the given table expression that are referred to by this SELECT */
      def colsOf(table: TableExpr): List[Column] =
        (cols.filter(_.table == table.refName) ++ joins.flatMap(_.colsOf(table))).distinct

      /** Does the result of this query contain lists of subobject? */
      def plural: Boolean = joins.exists(_.plural)
    }

    /** Representation of a UNION ALL of SQL SELECTs */
    case class SqlUnion(
      elems: List[SqlSelect],      // the underlying SELECTs
      aliases: List[String] = Nil  // column aliases if any
    ) extends SqlQuery {
      assert(elems.nonEmpty)

      /** The context for this query */
      val context = elems.head.context
      assert(elems.tail.forall(_.context == context))

      /** This query in the given context */
      def withContext(context: Context): SqlUnion = {
        copy(elems = elems.map(_.withContext(context)))
      }

      /** The union of the columns of the underlying SELECTs in the order they will be
       *  yielded as the columns of this UNION
       */
      lazy val alignedCols: List[Column] = elems.flatMap(_.cols).distinct

      /** The underlying SELECTs with their columns permuted into the aligned order
       *  with any missing columns padded as NULLs
       */
      lazy val alignedElems: List[SqlSelect] = {
        elems.map { elem =>
          val cols = alignedCols.map { col =>
            if (elem.cols.contains(col)) col
            else Column.NullColumn(col)
          }
          elem.copy(cols = cols, aliases = aliases)
        }
      }

      /** Execute this query in `F` */
      def fetch: F[Table] =
        self.fetch(toFragment, alignedCols.map(col => (true, col.codec)))

      /** The index of the given unaliased column in the result set */
      def index(col: Column): Int = {
        val i = alignedCols.indexOf(col)
        if (i < 0) throw new RuntimeException(s"Unmapped column ${col.column} of table ${col.table}")
        i
      }

      /** Add the given columns to this query */
      def addColumns(cols: List[Column]): SqlUnion =
        SqlUnion(elems.map(_.addColumns(cols)))

      /** Nest this query as a subobject in the enclosing `parentContext` */
      def nest(
        parentContext: Context,
        fieldName: String,
        resultName: String,
        extraCols: List[Column],
        extraJoins: List[SqlJoin],
        aliasedMappings: AliasedMappings
      ): (SqlQuery, AliasedMappings) = {
        val childTable: TableRef =
          aliasedMappings.parentTableForType(context).
            getOrElse(sys.error(s"No parent table for type ${context.tpe}"))

        val (am0, parentAlias) = aliasedMappings.aliasTable(context, childTable.refName)

        val joinCols = extraJoins.flatMap { join =>
          join.on.collect { case (_, c) if c.table == childTable.refName => c }
        }
        val thisWithJoinCols = this.addColumns(joinCols)

        val aliasedCols = am0.computeColumnAliases(parentAlias, thisWithJoinCols.alignedCols)
        val (am1, outerColumns) = am0.aliasColumns(context, thisWithJoinCols.alignedCols, aliasedCols)
        val innerAliases = outerColumns.map(_.column)
        val thisWithAliases = thisWithJoinCols.copy(aliases = innerAliases)
        val ref = SubqueryRef(thisWithAliases, parentAlias)

        val from =
          if (extraJoins.isEmpty) ref
          else
            am1.parentTableForType(parentContext).
              getOrElse(sys.error(s"No parent table for type ${parentContext.tpe}"))

        val cols = (extraCols ++ outerColumns).distinct

        val joins =
          extraJoins.map { join =>
            val newOn = join.on.map {
              case (p, c) if c.table == childTable.refName =>
                (p, am1.computeColumnAlias(parentAlias, c))
              case other => other
            }

            join.copy(child = ref, on = newOn)
          }

        val sel = SqlSelect(parentContext, from, cols, joins, Nil, Nil, None, None, false)
        (sel, am1)
      }

      /** Add WHERE, ORDER BY, OFFSET, and LIMIT to this query */
      def addFilterOrderByLimit(
        pred: Option[Predicate],
        oss: Option[List[OrderSelection[_]]],
        offset: Option[Int],
        limit: Option[Int],
        folQuery: Option[SqlSelect],
        aliasedMappings: AliasedMappings
      ): Option[(SqlUnion, AliasedMappings)] = {
        pred match {
          // Handle OrderBy and Limit programmatically for now
          case None => Some((this, aliasedMappings))
          case _ =>
            val (elems0, am0) =
              elems.foldLeft((List.empty[SqlSelect], aliasedMappings)) { case ((elems0, am), elem) =>
                elem.addFilterOrderByLimit(pred, None, None, None, folQuery, am) match {
                  case Some((elem0, am0)) => ((elem0 :: elems0), am0)
                  case None => ((elem :: elems0), am)
                }
              }
            Some((SqlUnion(elems0), am0))
        }
      }

      def toFragment: Fragment =
        alignedElems.map(_.toFragment).reduce((x, y) => Fragments.parentheses(x) |+| Fragments.const(" UNION ALL ") |+| Fragments.parentheses(y))
    }

    /** Representation of an SQL join */
    case class SqlJoin(
      parent:  String,                  // name of parent table
      child:   TableExpr,               // child table/subquery
      on:      List[(Column, Column)],  // join conditions
      plural:  Boolean,                 // does the result of this join contain a list of subobjects?
      lateral: Boolean,
      inner:   Boolean
    ) {
      assert(on.forall { case (p, c) => p.table == parent && c.table == child.refName })
      assert(child.name.map(!_.contains("_alias_")).getOrElse(true))

      /** Replace references to `from` with `to` */
      def subst(from: TableExpr, to: TableExpr): SqlJoin = {
        val newParent = if(parent == from.refName) to.refName else parent
        val newChild =
          if(child.refName != from.refName) child
          else {
            (child, to) match {
              case (sr: SubqueryRef, to: TableRef) => sr.copy(alias0 = to.refName)
              case _ => to
            }
          }
        val newOn = on.map { case (p, c) => (p.subst(from, to), c.subst(from, to)) }
        copy(parent = newParent, child = newChild, on = newOn)
      }

      def substParent(from: TableExpr, to: TableExpr): SqlJoin = {
        val newParent = if(parent == from.refName) to.refName else parent
        val newOn = on.map { case (p, c) => (p.subst(from, to), c) }
        copy(parent = newParent, on = newOn)
      }

      def substChild(from: TableExpr, to: TableExpr): SqlJoin = {
        val newChild =
          if(child.refName != from.refName) child
          else {
            (child, to) match {
              case (sr: SubqueryRef, to: TableRef) => sr.copy(alias0 = to.refName)
              case _ => to
            }
          }
        val newOn = on.map { case (p, c) => (p, c.subst(from, to)) }
        copy(child = newChild, on = newOn)
      }

      /** Return the columns of `table` referred to by the parent side of the conditions of this join */
      def colsOf(table: TableExpr): List[Column] =
        if (table.refName == parent) on.map(_._1)
        else Nil

      /** Return the columns of `table` referred to by the child side of the conditions of this join */
      def colsOfChild(table: TableExpr): List[Column] =
        if (table.refName == child.refName) on.map(_._2)
        else Nil

      def toFragment: Fragment = {
        val on0 = Fragments.and(on.map { case (p, c) => c.toFragment |+| Fragments.const(" = ") |+| p.toFragment }: _*)
        val onFrag = Fragments.const(s" ON ") |+| on0
        val join = if (inner) "INNER JOIN" else "LEFT JOIN"
        child match {
          case SubqueryRef(sq: SqlSelect, alias) if lateral =>
            val wheres = on.map { case (p, c) =>
              Where(c.subst(child, sq.table).toFragment |+| Fragments.const(" = ") |+| p.subst(child, sq.table).toFragment)
            }
            val sr = SubqueryRef(sq.copy(wheres = wheres ++ sq.wheres), alias)
            Fragments.const(s" $join LATERAL ") |+| sr.toFragment |+| onFrag

          case (_: TableRef) | (_: SubqueryRef) =>
            Fragments.const(s" $join ") |+| child.toFragment |+| onFrag
        }
      }
    }

    object SqlJoin {
      def unaliasTable(table: String): String =
        table.indexOf("_alias_") match {
          case -1 => table
          case n => table.take(n)
        }

      def fromJoins(child: TableRef, joins: List[Join], plural: Boolean, inner: Boolean): List[SqlJoin] =
        joins.map { j =>
          val child0 = if (j.child.table == child.refName) child else TableRef.fromRefName(j.child.table)
          new SqlJoin(j.parent.table, child0, List((j.parent, j.child)), plural, false, inner)
        }

      def checkOrdering(parent: String, joins: List[SqlJoin]): Boolean = {
        @tailrec
        def loop(joins: List[SqlJoin], seen: Set[String]): Boolean = {
          joins match {
            case Nil => true
            case hd :: tl =>
              seen.contains(hd.parent) && loop(tl, seen+hd.child.refName)
          }
        }
        loop(joins, Set(parent))
      }
    }

    /** Represents a WHERE clause */
    case class Where(frag: Fragment) {
      def toFragment: Fragment = frag
    }

    /** Represents an ORDER BY clause */
    case class Order(frag: Fragment) {
      def toFragment: Fragment = frag
    }
  }

  /** Represents the mapping of a GraphQL query to an SQL query */
  final class MappedQuery(
    query: SqlQuery,
    val aliasedMappings: AliasedMappings
  ) {
    /** Execute this query in `F` */
    def fetch: F[Table] = query.fetch

    lazy val fragment = query.toFragment

    /** The index of the given unaliased column in the result set */
    def index(col: Column): Int = query.index(col)

    private def project(row: Row, cols: List[Column]): Row =
      Row(cols.map(cr => row(index(cr))))

    private def select(row: Row, col: Column): Any =
      row(index(col))

    /** Return the value of the field `fieldName` in `context` from `table` */
    def selectAtomicField(context: Context, fieldName: String, table: Table): Result[Any] =
      aliasedMappings.columnForAtomicField(context, fieldName) match {
        case Some(col) =>
          table.map(row => select(row, col)).filterNot(_ == FailedJoin).distinct match {
            case Nil => FailedJoin.rightIor
            case List(value) => value.rightIor
            case multi =>
              val obj = context.tpe.dealias
              if (obj.variantField(fieldName) || obj.field(fieldName).map(_.isNullable).getOrElse(true))
                // if the field is a non-schema attribute we won't be able to discover whether
                // or not it's nullable. Instead we assume that the presense of a None would implies
                // nullability, hence stripping out Nones is justified.
                multi.filterNot(_ == None) match {
                  case Nil => None.rightIor
                  case List(value) => value.rightIor
                  case multi =>
                    mkErrorResult(s"Expected single value for field '$fieldName' of type $obj at ${context.path}, found $multi")
                }
              else
                mkErrorResult(s"Expected single value for field '$fieldName' of type $obj at ${context.path}, found $multi")
          }
        case None =>
          val obj = context.tpe.dealias
          mkErrorResult(s"Expected mapping for field '$fieldName' of type $obj")
      }

    /** Retain only rows of `table` which correspond to complete values of the context type */
    def stripNulls(context: Context, table: Table): Table = {
      aliasedMappings.keyColumnsForType(context) match {
        case Nil => table
        case cols =>
          table.filterNot(row => project(row, cols).elems.exists(_ == FailedJoin))
      }
    }

    /** Does `table` contain subobjects of the type of the `narrowedContext` type */
    def narrowsTo(narrowedContext: Context, table: Table): Boolean =
      aliasedMappings.keyColumnsForType(narrowedContext) match {
        case Nil => false
        case cols =>
          !table.exists(row => project(row, cols).elems.exists(_ == FailedJoin))
      }

    /** Yield a `Table` containing only subojects of the `narrowedContext` type */
    def narrow(narrowedContext: Context, table: Table): Table = {
      aliasedMappings.keyColumnsForType(narrowedContext) match {
        case Nil => table
        case cols =>
          table.filter(row => project(row, cols).elems.forall(_ != FailedJoin))
      }
    }

    /** Yield a list of `Tables` one for each of the subobjects of the context type
     *  contained in `table`.
     */
    def group(context: Context, table: Table): List[Table] = {
      aliasedMappings.keyColumnsForType(context) match {
        case Nil => table.map(List(_))
        case cols =>
          val nonNull = table.filterNot(row => project(row, cols).elems.exists(_ == FailedJoin))
          val numbered = nonNull.zipWithIndex
          val grouped = numbered.groupBy { case (row, _) => project(row, cols) }
          val ordered0 = grouped.toList.map { kv =>
            val sorted = kv._2.sortBy(_._2)
            val min = sorted.head._2
            (sorted.map(_._1), min)
          }
          val ordered1 = ordered0.sortBy(_._2).map(_._1)
          ordered1
      }
    }
  }

  object MappedQuery {
    def apply(q: Query, context: Context): Option[MappedQuery] = {
      def loop(q: Query, context: Context, aliasedMappings: AliasedMappings): Option[(SqlQuery, AliasedMappings)] = {
        import aliasedMappings._

        lazy val parentTable: TableRef =
          aliasedMappings.parentTableForType(context).getOrElse(sys.error(s"No parent table for type ${context.tpe}"))

        def group(queries: List[Query]): Option[(SqlQuery, AliasedMappings)] = {
          val (nodes, am) =
            queries.foldLeft((List.empty[SqlQuery], aliasedMappings)) {
              case ((nodes, am), q) =>
                loop(q, context, am) match {
                  case Some((n, am0)) =>
                    (n :: nodes, am0)
                  case None =>
                    (nodes, am)
                }
            }

          SqlQuery.combineAll(nodes).map { grouped => (grouped, am) }
        }

        q match {
          // Leaf or Json element: no subobjects
          case PossiblyRenamedSelect(Select(fieldName, _, child), _) if child == Empty || isJsonb(context, fieldName) =>
            val cols = columnsForLeaf(context, fieldName)
            val extraCols = keyColumnsForType(context) ++ discriminatorColumnsForType(context)
            Some((SqlSelect(context, parentTable, (cols ++ extraCols).distinct, Nil, Nil, Nil, None, None, false), aliasedMappings.seenTable(parentTable.refName)))

          // Non-leaf non-Json elememtn: compile subobject queries
          case PossiblyRenamedSelect(s@Select(fieldName, _, child), resultName) =>
            val fieldContext = context.forField(fieldName, resultName).getOrElse(sys.error(s"No field '$fieldName' of type ${context.tpe}"))
            joinsForSubObject(context, fieldName, resultName, containsNonLeafList(s, context)) match {
              case (am, sjoins) =>
                val extraCols = keyColumnsForType(context) ++ discriminatorColumnsForType(context)
                loop(child, fieldContext, am.seenTable(parentTable.refName)).map {
                  case (n0, am0) => n0.nest(context, fieldName, resultName, extraCols, sjoins, am0)
                }
            }

          case Group(queries) => group(queries)

          case GroupList(queries) => group(queries)

          case Wrap(_, child) =>
            loop(child, context, aliasedMappings)

          case Count(countName, child) =>
            def childContext(q: Query): Option[Context] =
              q match {
                case PossiblyRenamedSelect(Select(fieldName, _, _), resultName) =>
                  context.forField(fieldName, resultName)
                case FilterOrderByLimit(_, _, _, _, child) =>
                  childContext(child)
                case _ => None
              }

            val fieldContext = childContext(child).getOrElse(sys.error(s"No context for count of ${child}"))
            val countCol = columnForAtomicField(context, countName).getOrElse(sys.error(s"Count column $countName not defined"))

            loop(child, context, aliasedMappings).flatMap {
              case (sq: SqlSelect, _) =>
                sq.joins match {
                  case hd :: tl =>
                    val keyCols = keyColumnsForType(fieldContext)
                    val parentCols = hd.colsOf(parentTable)
                    val wheres = hd.on.map { case (p, c) => SqlQuery.Where(c.toFragment |+| Fragments.const(" = ") |+| p.toFragment) }
                    val ssq = sq.copy(table = hd.child, cols = List(Column.CountColumn(countCol, keyCols)), joins = tl, wheres = wheres)
                    val ssqCol = Column.SubqueryColumn(countCol, ssq)
                    Some((SqlSelect(context, parentTable, List(ssqCol) ++ parentCols, Nil, Nil, Nil, None, None, false), aliasedMappings))
                  case _ => None
                }
              case _ => None
            }

          case Rename(_, child) =>
            loop(child, context, aliasedMappings)

          case Unique(child) =>
            loop(child, context.asType(context.tpe.nonNull.list), aliasedMappings).map {
              case (node, am) => (node.withContext(context), am)
            }

          case FilterOrderByLimit(pred, oss, offset, lim, child) =>
            val wherePaths = pred.map(SqlQuery.wherePaths).getOrElse(Nil)
            val orderPaths = oss.map(_.map(_.term).collect { case path: Path => path.path }).getOrElse(Nil)
            val allPaths = (wherePaths ++ orderPaths).distinct

            val folQueries = if (allPaths.nonEmpty) Some(mkPathQuery(allPaths)) else None
            val folQuery = folQueries.map(mergeQueries)

            val sqlWherePaths =
              pred.map { p =>
                if (aliasedMappings.isSqlTerm(context, p)) Nil
                else wherePaths
              }.getOrElse(Nil)
            val mergedPaths = (sqlWherePaths ++ orderPaths).distinct
            val mergedChild =
              if (mergedPaths.isEmpty) child
              else mergeQueries(child :: mkPathQuery(mergedPaths))

            for {
              folNode    <- folQuery.map(q => loop(q, context, AliasedMappings.empty).map(_._1)).orElse(Some(None))
              folSelect  <- folNode.map(q => q match { case s: SqlSelect => Some(s) ; case _ => None }).orElse(Some(None))
              (node, am) <- loop(mergedChild, context, aliasedMappings)
              res        <- node.addFilterOrderByLimit(pred, oss, offset, lim, folSelect, am)
            } yield res

          case fol@(_: Filter | _: OrderBy | _: Offset | _: Limit) =>
            sys.error(s"Filter/OrderBy/Offset/Limit not matched by extractor: $fol")

          case _: Introspect =>
            val extraCols = keyColumnsForType(context) ++ discriminatorColumnsForType(context)
            Some((SqlSelect(context, parentTable, extraCols.distinct, Nil, Nil, Nil, None, None, false), aliasedMappings))

          case Environment(_, child) =>
            loop(child, context, aliasedMappings)

          case Narrow(subtpe, child) =>
            assert(subtpe <:< context.tpe.underlying)
            val subtpe0 = subtpe.withModifiersOf(context.tpe)
            val extraCols = keyColumnsForType(context) ++ discriminatorColumnsForType(context)

            loop(child, context.asType(subtpe0), aliasedMappings).map {
              case (n, am) =>
                (n.withContext(context).addColumns(extraCols), am)
            }

          case Empty | Skipped | Query.Component(_, _, _) | (_: Defer) | (_: UntypedNarrow) | (_: Skip) | (_: Select) =>
            None
        }
      }

      loop(q, context, AliasedMappings.empty).map { case (node, aliasedMappings) =>
        new MappedQuery(node, aliasedMappings)
      }
    }
  }

  /** Cursor positioned at a GraphQL result leaf */
  case class LeafCursor(context: Context, focus: Any, mapped: MappedQuery, parent: Option[Cursor], env: Env) extends Cursor {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def mkChild(context: Context = context, focus: Any = focus): LeafCursor =
      LeafCursor(context, focus, mapped, Some(this), Env.empty)

    def isLeaf: Boolean = tpe.isLeaf

    def asLeaf: Result[Json] =
      mapped.aliasedMappings.encoderForLeaf(tpe).map(enc => enc(focus).rightIor).getOrElse(
        focus match {
          case s: String => Json.fromString(s).rightIor
          case i: Int => Json.fromInt(i).rightIor
          case l: Long => Json.fromLong(l).rightIor
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
      case (ListType(tpe), it: List[_]) => it.map(f => mkChild(context.asType(tpe), focus = f)).rightIor
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
        case (NullableType(tpe), Some(v)) => Some(mkChild(context.asType(tpe), focus = v)).rightIor
        case _ => mkErrorResult(s"Not nullable at ${context.path}")
      }

    def narrowsTo(subtpe: TypeRef): Boolean = false
    def narrow(subtpe: TypeRef): Result[Cursor] =
      mkErrorResult(s"Cannot narrow $tpe to $subtpe")

    def hasField(fieldName: String): Boolean = false
    def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      mkErrorResult(s"Cannot select field '$fieldName' from leaf type $tpe")
  }

  /** Cursor positioned at a GraphQL result non-leaf */
  case class SqlCursor(context: Context, focus: Any, mapped: MappedQuery, parent: Option[Cursor], env: Env) extends Cursor {
    val aliasedMappings = mapped.aliasedMappings

    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def mkChild(context: Context = context, focus: Any = focus): SqlCursor =
      SqlCursor(context, focus, mapped, Some(this), Env.empty)

    def asTable: Result[Table] = focus match {
      case table@((_: Row) :: _ | Nil) => table.asInstanceOf[Table].rightIor
      case _ => mkErrorResult(s"Not a table")
    }

    def isLeaf: Boolean = false
    def asLeaf: Result[Json] =
      mkErrorResult(s"Not a leaf: $tpe")

    def isList: Boolean =
      tpe.isList

    def asList: Result[List[Cursor]] =
      tpe.item.map(_.dealias).map(itemTpe =>
        asTable.map { table =>
          val itemContext = context.asType(itemTpe)
          val keyCols = aliasedMappings.keyColumnsForType(itemContext)

          // If this mapping is a list of child objects then its fields came from an outer join. If
          // there are no children then all keys defined in the mapping will have the `FailedJoin`
          // value.
          val isEmpty: Boolean =
            keyCols.forall { cr =>
              val ix = mapped.index(cr)
              table.forall(r => r(ix) == FailedJoin)
            }

          // Sanity check: isEmpty implies that we had zero rows, or one row with failed joins.
          if (isEmpty)
            assert(table.sizeCompare(1) <= 0)

          // Done!
          if (isEmpty) Nil
          else {
            mapped.group(itemContext, table).map(table => mkChild(itemContext, focus = table))
          }
        }
      ).getOrElse(mkErrorResult(s"Not a list: $tpe"))

    def isNullable: Boolean =
      tpe.isNullable

    def asNullable: Result[Option[Cursor]] =
      (tpe, focus) match {
        case (NullableType(_), Nil) => None.rightIor
        case (NullableType(tpe), _) => Some(mkChild(context.asType(tpe))).rightIor // non-nullable column as nullable schema type (ok)
        case _ => mkErrorResult(s"Not nullable at ${context.path}")
      }

    def narrowsTo(subtpe: TypeRef): Boolean = {
      val ctpe =
        aliasedMappings.discriminatorForType(context) match {
          case Some(discriminator) => discriminator.discriminate(this).getOrElse(tpe)
          case _ => tpe
        }
      if (ctpe =:= tpe)
        asTable.map(table => mapped.narrowsTo(context.asType(subtpe), table)).right.getOrElse(false)
      else ctpe <:< subtpe
    }

    def narrow(subtpe: TypeRef): Result[Cursor] = {
      if (narrowsTo(subtpe)) {
        val narrowedContext = context.asType(subtpe)
        asTable.map { table =>
          mkChild(context = narrowedContext, focus = mapped.narrow(narrowedContext, table))
        }
      } else mkErrorResult(s"Cannot narrow $tpe to $subtpe")
    }

    def hasField(fieldName: String): Boolean = tpe.hasField(fieldName)

    def field(fieldName: String, resultName: Option[String]): Result[Cursor] = {
      tpe.underlyingObject.map { obj =>
        val fieldContext = context.forFieldOrAttribute(fieldName, resultName)
        val fieldTpe = fieldContext.tpe

        aliasedMappings.fieldMappingType(context, fieldName).toRightIor(mkOneError(s"No field mapping for field '$fieldName' of type $obj")).flatMap {
          case CursorFieldMapping(f) =>
            f(this).map(res => LeafCursor(fieldContext, res, mapped, Some(this), Env.empty))

          case CursorFieldJsonMapping(f) =>
            f(this).map(res => CirceCursor(fieldContext, focus = res, parent = Some(this), env = Env.empty))

          case JsonFieldMapping =>
            asTable.flatMap { table =>
              def mkCirceCursor(f: Json): Result[Cursor] =
                CirceCursor(fieldContext, focus = f, parent = Some(this), env = Env.empty).rightIor
              mapped.selectAtomicField(context, fieldName, table).flatMap(_ match {
                case Some(j: Json) if fieldTpe.isNullable => mkCirceCursor(j)
                case None => mkCirceCursor(Json.Null)
                case j: Json if !fieldTpe.isNullable => mkCirceCursor(j)
                case other =>
                  mkErrorResult(s"$fieldTpe: expected jsonb value found ${other.getClass}: $other")
              })
            }

          case LeafFieldMapping =>
            asTable.flatMap(table =>
              mapped.selectAtomicField(context, fieldName, table).map { leaf =>
                val leafFocus = leaf match {
                  case Some(f) if tpe.variantField(fieldName) && !fieldTpe.isNullable => f
                  case other => other
                }
                LeafCursor(fieldContext, leafFocus, mapped, Some(this), Env.empty)
              }
            )

          case ObjectFieldMapping =>
            asTable.map { table =>
              val focussed = mapped.stripNulls(fieldContext, table)
              mkChild(context = fieldContext, focus = focussed)
            }
        }
      }.getOrElse(mkErrorResult(s"Type $tpe has no field '$fieldName'"))
    }
  }
}
