// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

import cats.{ Apply, Monad, Monoid }
import cats.data.{ Chain, Ior, IorT, Kleisli, NonEmptyChain }
import cats.kernel.{ Eq, Order }
import cats.implicits._
import io.circe.Json
import io.circe.literal.JsonStringContext

import Query._, Predicate.In
import QueryInterpreter.{ mkErrorResult, ProtoJson }

/** GraphQL query Algebra */
sealed trait Query {
  /** Groups this query with its argument, Groups on either side are merged */
  def ~(query: Query): Query = (this, query) match {
    case (Group(hd), Group(tl)) => Group(hd ++ tl)
    case (hd, Group(tl)) => Group(hd :: tl)
    case (Group(hd), tl) => Group(hd :+ tl)
    case (hd, tl) => Group(List(hd, tl))
  }

  /** Yields a String representation of this query */
  def render: String
}

object Query {
  /** Select field `name` given arguments `args` and continue with `child` */
  case class Select(name: String, args: List[Binding], child: Query = Empty) extends Query {
    def eliminateArgs(elim: Query => Query): Query = copy(args = Nil, child = elim(child))

    def transformChild(f: Query => Query): Query = copy(child = f(child))

    def render = {
      val rargs = if(args.isEmpty) "" else s"(${args.map(_.render).mkString(", ")})"
      val rchild = if(child == Empty) "" else s" { ${child.render} }"
      s"$name$rargs$rchild"
    }
  }

  /** A Group of sibling queries at the same level */
  case class Group(queries: List[Query]) extends Query {
    def render = queries.map(_.render).mkString("{", ", ", "}")
  }

  /** A Group of sibling queries as a list */
  case class GroupList(queries: List[Query]) extends Query {
    def render = queries.map(_.render).mkString("[", ", ", "]")
  }

  /** Picks out the unique element satisfying `pred` and continues with `child` */
  case class Unique(pred: Predicate, child: Query) extends Query {
    def render = s"<unique: $pred ${child.render}>"
  }

  /** Retains only elements satisfying `pred` and continuse with `child` */
  case class Filter(pred: Predicate, child: Query) extends Query {
    def render = s"<filter: $pred ${child.render}>"
  }

  /** Identifies a component boundary.
   *  `join` is applied to the current cursor and `child` yielding a continuation query which will be
   *  evaluated by the interpreter identified by `componentId`.
   */
  case class Component[F[_]](mapping: Mapping[F], join: (Cursor, Query) => Result[Query], child: Query) extends Query {
    def render = s"<component: $mapping ${child.render}>"
  }

  case class Introspect(schema: Schema, child: Query) extends Query {
    def render = s"<introspect: ${child.render}>"
  }

  /** A deferred query.
   *  `join` is applied to the current cursor and `child` yielding a continuation query which will be
   *  evaluated by the current interpreter in its next stage.
   */
  case class Defer(join: (Cursor, Query) => Result[Query], child: Query, rootTpe: Type) extends Query {
    def render = s"<defer: ${child.render}>"
  }

  case class Context(path: List[String], child: Query) extends Query {
    def render = s"<context: $path ${child.render}>"
  }

  /**
   * Wraps the result of `child` as a field named `name` of an enclosing object.
   */
  case class Wrap(name: String, child: Query) extends Query {
    def render = {
      val rchild = if(child == Empty) "" else s" { ${child.render} }"
      s"$name$rchild"
    }
  }

  /**
   * Rename the topmost field of `sel` to `name`.
   */
  case class Rename(name: String, child: Query) extends Query {
    def render = s"<rename: $name ${child.render}>"
  }

  /**
   * Untyped precursor of `Narrow`.
   *
   * Trees of this type will be replaced by a corresponding `Narrow` by
   * `SelectElaborator`.
   */
  case class UntypedNarrow(tpnme: String, child: Query) extends Query {
    def render = s"<narrow: $tpnme ${child.render}>"
  }

  /**
   * The result of `child` if the focus is of type `subtpe`, `Empty` otherwise.
   */
  case class Narrow(subtpe: TypeRef, child: Query) extends Query {
    def render = s"<narrow: $subtpe ${child.render}>"
  }

  case class Skip(sense: Boolean, cond: Value, child: Query) extends Query {
    def render = s"<skip: $sense $cond ${child.render}>"
  }

  case class Limit(num: Int, child: Query) extends Query {
    def render = s"<limit: $num ${child.render}>"
  }

  case class OrderBy(selections: OrderSelections, child: Query) extends Query {
    def render = s"<order-by: $selections ${child.render}>"
  }

  case class OrderSelections(selections: List[OrderSelection[_]]) {
    def order(lc: List[Cursor]): List[Cursor] = {
      def cmp(x: Cursor, y: Cursor): Int = {
        @tailrec
        def loop(sels: List[OrderSelection[_]]): Int =
          sels match {
            case Nil => 0
            case hd :: tl =>
              hd(x, y) match {
                case 0 => loop(tl)
                case ord => ord
              }
          }

        loop(selections)
      }

      lc.sortWith((x, y) => cmp(x, y) < 0)
    }
  }

  case class OrderSelection[T: Order](t: Term[T], ascending: Boolean = true, nullsLast: Boolean = true) {
    def apply(x: Cursor, y: Cursor): Int = {
      def deref(c: Cursor): Option[T] =
        if (c.isNullable) c.asNullable.getOrElse(None).flatMap(t(_).toOption)
        else t(c).toOption

      (deref(x), deref(y)) match {
        case (None, None) => 0
        case (_, None) => (if (nullsLast) -1 else 1)
        case (None, _) => (if (nullsLast) 1 else -1)
        case (Some(x0), Some(y0)) =>
          val ord = Order[T].compare(x0, y0)
          if (ascending) ord
          else -ord
      }
    }
  }

  case class GroupBy(discriminator: GroupDiscriminator[_], child: Query) extends Query {
    def render = s"<group-by: $discriminator ${child.render}>"
  }

  case class GroupDiscriminator[T: Eq](t: Term[T], ds: List[T]) {
    def group(cs: List[Cursor]): List[List[Cursor]] = {
      def deref(c: Cursor): Option[T] =
        if (c.isNullable) c.asNullable.getOrElse(None).flatMap(t(_).toOption)
        else t(c).toOption

      val tagged: List[(Cursor, Int)] = cs.map { c =>
        (c, deref(c).map { t => ds.indexOf(t) }.getOrElse(-1))
      }

      val sorted: List[(Cursor, Int)] = tagged.sortBy(_._2).dropWhile(_._2 == -1)

      val ngroups = ds.length

      def loop(cis: List[(Cursor, Int)], prev: Int, acc0: List[Cursor], acc1: List[List[Cursor]]): List[List[Cursor]] =
        cis match {
          case Nil =>
            val pad = List.fill(ngroups-prev-1)(Nil)
            (pad ++ (acc0.reverse :: acc1)).reverse
          case (c, i) :: tl if i == prev => loop(tl, i, c :: acc0, acc1)
          case (c, i) :: tl if i == prev+1 => loop(tl, i, List(c), acc0.reverse :: acc1)
          case cis => loop(cis, prev+1, Nil, acc0.reverse :: acc1)
        }

      loop(sorted, 0, Nil, Nil)
    }
  }

  /** The terminal query */
  case object Empty extends Query {
    def render = ""
  }

  case class Binding(name: String, value: Value) {
    def render: String = s"$name: $value"
  }

  type UntypedVarDefs = List[UntypedVarDef]
  type VarDefs = List[InputValue]
  type Env = Map[String, (Type, Value)]

  case class UntypedVarDef(name: String, tpe: Ast.Type, default: Option[Value])

  object PossiblyRenamedSelect {
    def apply(sel: Select, resultName: String): Query = sel match {
      case Select(`resultName`, _, _) => sel
      case _ => Rename(resultName, sel)
    }

    def unapply(q: Query): Option[(Select, String)] =
      q match {
        case Rename(name, sel: Select) => Some((sel, name))
        case sel: Select => Some((sel, sel.name))
        case _ => None
      }
  }

  def renameRoot(q: Query, rootName: String): Option[Query] = q match {
    case Rename(_, sel@Select(`rootName`, _, _)) => Some(sel)
    case r@Rename(`rootName`, _)                 => Some(r)
    case Rename(_, sel: Select)                  => Some(Rename(rootName, sel))
    case sel@Select(`rootName`, _, _)            => Some(sel)
    case sel: Select                             => Some(Rename(rootName, sel))
    case w@Wrap(`rootName`, _)                   => Some(w)
    case w: Wrap                                 => Some(w.copy(name = rootName))
    case _ => None
  }

  def rootName(q: Query): Option[String] = q match {
    case Select(name, _, _)       => Some(name)
    case Wrap(name, _)            => Some(name)
    case Rename(name, _)          => Some(name)
    case _                        => None
  }
}

/**
 * A reified function over a `Cursor`.
 *
 * Query interpreters will typically need to introspect predicates (eg. in the doobie module
 * we need to be able to construct where clauses from predicates over fields/attributes), so
 * these cannot be arbitrary functions `Cursor => Boolean`.
 */
trait Term[T] extends Product with Serializable {
  def apply(c: Cursor): Result[T]
}

trait Predicate extends Term[Boolean]

object Predicate {
  object ScalarFocus {
    def unapply(c: Cursor): Option[Any] =
      if (c.isLeaf) Some(c.focus)
      else if (c.isNullable)
        c.asNullable match {
          case Ior.Right(Some(c)) => unapply(c)
          case _ => None
        }
      else None
  }

  case object True extends Predicate {
    def apply(c: Cursor): Result[Boolean] = true.rightIor
  }

  case object False extends Predicate {
    def apply(c: Cursor): Result[Boolean] = false.rightIor
  }

  def and(props: List[Predicate]): Predicate = {
    @tailrec
    def loop(props: List[Predicate], acc: Predicate): Predicate =
      props match {
        case Nil => acc
        case False :: _ => False
        case True :: tl => loop(tl, acc)
        case hd :: tl if acc == True => loop(tl, hd)
        case hd :: tl => loop(tl, And(hd, acc))
      }
    loop(props, True)
  }

  def or(props: List[Predicate]): Predicate = {
    @tailrec
    def loop(props: List[Predicate], acc: Predicate): Predicate =
      props match {
        case Nil => acc
        case True :: _ => True
        case False :: tl => loop(tl, acc)
        case hd :: tl if acc == False => loop(tl, hd)
        case hd :: tl => loop(tl, Or(hd, acc))
      }
    loop(props, False)
  }

  case class Const[T](v: T) extends Term[T] {
    def apply(c: Cursor): Result[T] = v.rightIor
  }

  case class Project(path: List[String], pred: Predicate) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = {
      c.listPath(path).flatMap(cs => Cursor.flatten(cs).flatMap(_.traverse(pred(_)).map(bs => bs.exists(identity))))
    }
  }

  sealed trait Path {
    def path: List[String]
    def extend(prefix: List[String]): Path
  }

  case class FieldPath[T](val path: List[String]) extends Term[T] with Path {
    def apply(c: Cursor): Result[T] =
      c.listPath(path) match {
        case Ior.Right(List(ScalarFocus(a: T @unchecked))) => a.rightIor
        case other => mkErrorResult(s"Expected exactly one element for path $path found $other")
      }

    def extend(prefix: List[String]): Path =
      FieldPath(prefix ++ path)
  }

  case class AttrPath[T](val path: List[String]) extends Term[T] with Path {
    def apply(c: Cursor): Result[T] =
      c.attrListPath(path) match {
        case Ior.Right(List(a: T @unchecked)) => a.rightIor
        case other => mkErrorResult(s"Expected exactly one element for path $path found $other")
      }

    def extend(prefix: List[String]): Path =
      AttrPath(prefix ++ path)
  }

  case class CollectFieldPath[T](val path: List[String]) extends Term[List[T]] with Path {
    def apply(c: Cursor): Result[List[T]] =
      c.flatListPath(path).map(_.map { case ScalarFocus(f: T @unchecked) => f })

    def extend(prefix: List[String]): Path =
      CollectFieldPath(prefix ++ path)
  }

  case class CollectAttrPath[T](val path: List[String]) extends Term[List[T]] with Path {
    def apply(c: Cursor): Result[List[T]] =
      c.attrListPath(path).map(_.asInstanceOf[List[T]])

    def extend(prefix: List[String]): Path =
      CollectAttrPath(prefix ++ path)
  }

  case class And(x: Predicate, y: Predicate) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_ && _)
  }

  case class Or(x: Predicate, y: Predicate) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_ || _)
  }

  case class Not(x: Predicate) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = x(c).map(! _)
  }

  case class Eql[T: Eq](x: Term[T], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_ === _)
    def eqInstance: Eq[T] = implicitly[Eq[T]]
  }

  case class NEql[T: Eq](x: Term[T], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_ =!= _)
  }

  case class Contains[T: Eq](x: Term[List[T]], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))((xs, y0) => xs.exists(_ === y0))
  }

  case class Lt[T: Order](x: Term[T], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_.compare(_) < 0)
  }

  case class LtEql[T: Order](x: Term[T], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_.compare(_) <= 0)
  }

  case class Gt[T: Order](x: Term[T], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_.compare(_) > 0)
  }

  case class GtEql[T: Order](x: Term[T], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_.compare(_) >= 0)
  }

  case class In[T: Eq](x: Term[T], y: List[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = x(c).map(y.contains(_))

    def mkDiscriminator: GroupDiscriminator[T] = GroupDiscriminator(x, y)
  }

  object In {
    def fromEqls[T](eqls: List[Eql[T]]): Option[In[T]] = {
      val paths = eqls.map(_.x).distinct
      val consts = eqls.collect { case Eql(_, Const(c)) => c }
      if (eqls.map(_.x).distinct.size == 1 && consts.size == eqls.size)
        Some(In(paths.head, consts)(eqls.head.eqInstance))
      else
        None
    }
  }

  case class AndB(x: Term[Int], y: Term[Int]) extends Term[Int] {
    def apply(c: Cursor): Result[Int] = Apply[Result].map2(x(c), y(c))(_ & _)
  }

  case class OrB(x: Term[Int], y: Term[Int]) extends Term[Int] {
    def apply(c: Cursor): Result[Int] = Apply[Result].map2(x(c), y(c))(_ | _)
  }

  case class XorB(x: Term[Int], y: Term[Int]) extends Term[Int] {
    def apply(c: Cursor): Result[Int] = Apply[Result].map2(x(c), y(c))(_ ^ _)
  }

  case class NotB(x: Term[Int]) extends Term[Int] {
    def apply(c: Cursor): Result[Int] = x(c).map(~ _)
  }

  case class Matches(x: Term[String], r: Regex) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = x(c).map(r.matches(_))
  }

  case class StartsWith(x: Term[String], prefix: String) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = x(c).map(_.startsWith(prefix))
  }

  case class ToUpperCase(x: Term[String]) extends Term[String] {
    def apply(c: Cursor): Result[String] = x(c).map(_.toUpperCase)
  }

  case class ToLowerCase(x: Term[String]) extends Term[String] {
    def apply(c: Cursor): Result[String] = x(c).map(_.toLowerCase)
  }
}

class QueryInterpreter[F[_] : Monad](mapping: Mapping[F]) {

  /** Interpret `query` with expected type `rootTpe`.
   *
   *  The query is fully interpreted, including deferred or staged
   *  components.
   *
   *  The resulting Json value should include standard GraphQL error
   *  information in the case of failure.
   */
  def run(query: Query, rootTpe: Type): F[Json] =
    runRoot(query, rootTpe).map(QueryInterpreter.mkResponse)

  /** Interpret `query` with expected type `rootTpe`.
   *
   *  The query is fully interpreted, including deferred or staged
   *  components.
   *
   *  Errors are accumulated on the `Left` of the result.
   */
  def runRoot(query: Query, rootTpe: Type): F[Result[Json]] = {
    val rootQueries =
      query match {
        case Group(queries) => queries
        case query => List(query)
      }

    val introQueries = rootQueries.collect { case i: Introspect => i }
    val introResults =
      introQueries.map {
        case Introspect(schema, query) =>
          val interp = Introspection.interpreter(schema)
          interp.runRootValue(query, Introspection.schema.queryType)
      }

    val nonIntroQueries = rootQueries.filter { case _: Introspect => false ; case _ => true }
    val nonIntroResults = runRootValues(nonIntroQueries.zip(Iterator.continually(rootTpe)))

    val mergedResults: F[Result[ProtoJson]] =
      nonIntroResults.map {
        case (nonIntroErrors, nonIntroValues) =>

          val mergedErrors = introResults.foldLeft(nonIntroErrors) {
            case (acc, res) => res.left match {
              case Some(errs) => acc ++ errs.toChain
              case None => acc
            }
          }

          @tailrec
          def merge(qs: List[Query], is: List[Result[ProtoJson]], nis: List[ProtoJson], acc: List[ProtoJson]): List[ProtoJson] =
            ((qs, is, nis): @unchecked) match {
              case (Nil, _, _) => acc
              case ((_: Introspect) :: qs, i :: is, nis) =>
                val acc0 = i.right match {
                  case Some(r) => r :: acc
                  case None => acc
                }
                merge(qs, is, nis, acc0)
              case (_ :: qs, is, ni :: nis) =>
                merge(qs, is, nis, ni :: acc)
            }

          val mergedValues = ProtoJson.mergeObjects(merge(rootQueries, introResults, nonIntroValues, Nil).reverse)

          NonEmptyChain.fromChain(mergedErrors) match {
            case Some(errs) => Ior.Both(errs, mergedValues)
            case None => Ior.Right(mergedValues)
          }
      }
    (for {
      pvalue <- IorT(mergedResults)
      value  <- IorT(QueryInterpreter.complete[F](pvalue))
    } yield value).value
  }

  /** Interpret `query` with expected type `rootTpe`.
   *
   *  At most one stage will be run and the result may contain deferred
   *  components.
   *
   *  Errors are accumulated on the `Left` of the result.
   */
  def runRootValue0(query: Query, path: List[String], rootTpe: Type): F[Result[ProtoJson]] =
    query match {
      case Context(cpath, child) =>
        runRootValue0(child, cpath ++ path, rootTpe)

      case Select("__staged", _, child) =>
        (for {
          cursor <- IorT(mapping.rootCursor(path, rootTpe, "__staged", child))
          value  <- IorT(runValue(Wrap("__staged", child), rootTpe.list, cursor).pure[F])
        } yield
          ProtoJson.unpackObject(value) match {
            case Some(Nil) => ProtoJson.fromValues(Nil)
            case Some(List(unpacked)) => unpacked
          }
        ).value

      case PossiblyRenamedSelect(Select(fieldName, _, child), resultName) =>
        (for {
          cursor <- IorT(mapping.rootCursor(path, rootTpe, fieldName, child))
          value  <- IorT(runValue(Wrap(resultName, child), rootTpe.field(fieldName), cursor).pure[F])
        } yield value).value

      case q@GroupBy(_, Select(fieldName, _, child)) =>
        val elemTpe = rootTpe.item
        (for {
          cursor <- IorT(mapping.rootCursor(path, elemTpe, fieldName, child))
          value  <- IorT(runValue(q, rootTpe, cursor).pure[F])
        } yield value).value

      case Wrap(_, Component(mapping, _, child)) =>
        mapping.asInstanceOf[Mapping[F]].interpreter.runRootValue(child, rootTpe)

      case _ =>
        mkErrorResult(s"Bad root query '${query.render}' in QueryInterpreter").pure[F]
    }

  def runRootValue(query: Query, rootTpe: Type): F[Result[ProtoJson]] = runRootValue0(query, Nil, rootTpe)

  /** Interpret multiple queries with respect to their expected types.
   *
   *  Each query is interpreted with respect to the expected type it is
   *  paired with. The result list is aligned with the argument list
   *  query list. For each query at most one stage will be run and the
   *  corresponding result may contain deferred components.
   *
   *  Errors are aggregated across all the argument queries and are
   *  accumulated on the `Left` of the result.
   *
   *  This method is typically called at the end of a stage to evaluate
   *  deferred subqueries in the result of that stage. These will be
   *  grouped by and passed jointly to the responsible interpreter in
   *  the next stage using this method. Interpreters which are able
   *  to benefit from combining queries may do so by overriding this
   *  method to implement their specific combinging logic.
   */
  def runRootValues(queries: List[(Query, Type)]): F[(Chain[Json], List[ProtoJson])] =
    queries.traverse((runRootValue _).tupled).map { rs =>
      (rs.foldLeft((Chain.empty[Json], List.empty[ProtoJson])) {
        case ((errors, elems), elem) =>
          elem match {
            case Ior.Left(errs) => (errs.toChain ++ errors, ProtoJson.fromJson(Json.Null) :: elems)
            case Ior.Right(elem) => (errors, elem :: elems)
            case Ior.Both(errs, elem) => (errs.toChain ++ errors, elem :: elems)
          }
      }).map(_.reverse)
    }

  def cursorCompatible(tpe: Type, cursorTpe: Type): Boolean = {
    def strip(tpe: Type): Type =
      tpe.dealias match {
        case NullableType(tpe) => strip(tpe)
        case ListType(tpe) => strip(tpe)
        case _ => tpe
      }

    cursorTpe =:= NoType || (strip(tpe) nominal_=:= strip(cursorTpe))
  }

  /**
   * Interpret `query` against `cursor`, yielding a collection of fields.
   *
   * If the query is valid, the field subqueries will all be valid fields
   * of the enclosing type `tpe` and the resulting fields may be used to
   * build a Json object of type `tpe`. If the query is invalid errors
   * will be returned on the left hand side of the result.
   */
  def runFields(query: Query, tpe: Type, cursor: Cursor): Result[List[(String, ProtoJson)]] =
    if (!cursorCompatible(tpe, cursor.tpe))
      mkErrorResult(s"Mismatched query and cursor type in runFields: $tpe ${cursor.tpe}")
    else {
      (query, tpe.dealias) match {
        case (Narrow(tp1, child), _) =>
          if (!cursor.narrowsTo(tp1)) Nil.rightIor
          else
            for {
              c      <- cursor.narrow(tp1)
              fields <- runFields(child, tp1, c)
            } yield fields

        case (Introspect(schema, PossiblyRenamedSelect(Select("__typename", Nil, Empty), resultName)), tpe: NamedType) =>
          (tpe match {
            case o: ObjectType => Some(o.name)
            case i: InterfaceType =>
              (schema.types.collectFirst {
                case o: ObjectType if o <:< i && cursor.narrowsTo(schema.ref(o.name)) => o.name
              })
            case u: UnionType =>
              (u.members.map(_.dealias).collectFirst {
                case nt: NamedType if cursor.narrowsTo(schema.ref(nt.name)) => nt.name
              })
            case _ => None
          }) match {
            case Some(name) =>
              List((resultName, ProtoJson.fromJson(Json.fromString(name)))).rightIor
            case None =>
              mkErrorResult(s"'__typename' cannot be applied to non-selectable type '$tpe'")
          }

        case (PossiblyRenamedSelect(sel, resultName), NullableType(tpe)) =>
          cursor.asNullable.sequence.map { rc =>
            for {
              c      <- rc
              fields <- runFields(sel, tpe, c)
            } yield fields
          }.getOrElse(List((resultName, ProtoJson.fromJson(Json.Null))).rightIor)

        case (PossiblyRenamedSelect(Select(fieldName, _, child), resultName), tpe) =>
          for {
            c     <- cursor.field(fieldName)
            value <- runValue(child, tpe.field(fieldName), c)
          } yield List((resultName, value))

        case (Rename(resultName, Wrap(_, child)), tpe) =>
          for {
            value <- runValue(child, tpe, cursor)
          } yield List((resultName, value))

        case (Wrap(fieldName, child), tpe) =>
          for {
            value <- runValue(child, tpe, cursor)
          } yield List((fieldName, value))

        case (Group(siblings), _) =>
          siblings.flatTraverse(query => runFields(query, tpe, cursor))

        case _ =>
          mkErrorResult(s"runFields failed: { ${query.render} } $tpe")
      }
    }

  def runList(query: Query, tpe: Type, cursors: List[Cursor], f: Kleisli[Result, List[Cursor], List[Cursor]]): Result[ProtoJson] =
    if (cursors.exists(cursor => !cursorCompatible(tpe, cursor.tpe)))
      mkErrorResult(s"Mismatched query and cursor type in runList: $tpe ${cursors.map(_.tpe)}")
    else {
      query match {
        case Filter(pred, child) =>
          runList(child, tpe, cursors, f.compose(_.filterA(pred(_))))

        case Limit(num, child) =>
          runList(child, tpe, cursors, f.compose(_.take(num).rightIor))

        case OrderBy(selections, child) =>
          runList(child, tpe, cursors, f.compose(selections.order(_).rightIor))

        case _ =>
          f.run(cursors).flatMap(lc =>
            lc.traverse(c => runValue(query, tpe, c)).map(ProtoJson.fromValues)
          )
      }
    }

  /**
   * Interpret `query` against `cursor` with expected type `tpe`.
   *
   * If the query is invalid errors will be returned on teh left hand side
   * of the result.
   */
  def runValue(query: Query, tpe: Type, cursor: Cursor): Result[ProtoJson] =
    if (!cursorCompatible(tpe, cursor.tpe))
      mkErrorResult(s"Mismatched query and cursor type in runValue: $tpe ${cursor.tpe}")
    else {
      def mkResult[T](ot: Option[T]): Result[T] = ot match {
        case Some(t) => t.rightIor
        case None => mkErrorResult(s"Join continuation has unexpected shape")
      }

      (query, tpe.dealias) match {
        case (Context(_, child), tpe) =>
          runValue(child, tpe, cursor)

        case (Wrap(_, Component(_, _, _)), ListType(tpe)) =>
          // Keep the wrapper with the component when going under the list
          cursor.asList.flatMap(lc =>
            lc.traverse(c => runValue(query, tpe, c)).map(ProtoJson.fromValues)
          )

        case (Wrap(_, Defer(_, _, _)), _) if cursor.isNull =>
          ProtoJson.fromJson(Json.Null).rightIor

        case (Wrap(fieldName, child), _) =>
          for {
            pvalue <- runValue(child, tpe, cursor)
          } yield ProtoJson.fromFields(List((fieldName, pvalue)))

        case (Component(mapping, join, PossiblyRenamedSelect(child, resultName)), tpe) =>
          val interpreter = mapping.interpreter
          join(cursor, child).flatMap {
            case GroupList(conts) =>
              conts.traverse { case cont =>
                for {
                  componentName <- mkResult(rootName(cont))
                } yield
                  ProtoJson.select(
                    ProtoJson.staged(interpreter, cont, JoinType(componentName, tpe.field(child.name).item)),
                    componentName
                  )
              }.map(ProtoJson.fromValues)

            case cont =>
              for {
                componentName <- mkResult(rootName(cont))
                renamedCont   <- mkResult(renameRoot(cont, resultName))
              } yield ProtoJson.staged(interpreter, renamedCont, JoinType(componentName, tpe.field(child.name)))
          }

        case (Defer(join, child, rootTpe), _) =>
          def stage(cursor: Cursor) =
            for {
              cont <- join(cursor, child)
            } yield ProtoJson.staged(this, cont, rootTpe)

          if (cursor.isNullable)
            cursor.asNullable match {
              case Ior.Right(Some(c)) => stage(c)
              case _ => ProtoJson.fromJson(Json.Null).rightIor
            }
          else stage(cursor)

        case (Unique(pred, child), _) =>
          val cursors =
            if (cursor.isNullable)
              cursor.asNullable.flatMap {
                case None => Nil.rightIor
                case Some(c) => c.asList
              }
            else cursor.asList

          cursors.flatMap(_.filterA(pred(_))).flatMap(lc =>
            lc match {
              case List(c) => runValue(child, tpe.nonNull, c)
              case Nil if tpe.isNullable => ProtoJson.fromJson(Json.Null).rightIor
              case Nil => mkErrorResult(s"No match")
              case _ => mkErrorResult(s"Multiple matches")
            }
          )

        case (GroupBy(discriminator, Select(_, _, child)), ListType(ltpe)) =>
          val cursors =
            if (cursor.isNullable)
              cursor.asNullable.flatMap {
                case None => Nil.rightIor
                case Some(c) => c.asList
              }
            else cursor.asList

          val grouped: Result[List[List[Cursor]]] = cursors.map(cs => discriminator.group(cs))

          object PArr {
            def unapply(p: ProtoJson): Option[List[ProtoJson]] =
              p match {
                case ProtoJson.ProtoArray(elems) => Some(elems)
                case j: Json if j.isArray => j.asArray.map(ja => ja.toList.asInstanceOf[List[ProtoJson]])
                case _ => None
              }
          }

          val gr: Result[List[ProtoJson]] = grouped.flatMap(_.traverse { cs =>
            if(cs.isEmpty) {
              // If this group is empty we need to evaluate the continuation
              // relative to an empty cursor to create the appropriate empty
              // child
              child match {
                case Filter(In(_, _), cont) => runValue(cont, ltpe, Cursor.EmptyCursor(ltpe, Nil))
                case _ => ProtoJson.fromJson(Json.Null).rightIor
              }
            } else
              runList(child, ltpe, cs, Kleisli(_.rightIor)).flatMap { orig =>
                orig match {
                  case PArr(Nil) => ProtoJson.fromJson(Json.Null).rightIor
                  case PArr(List(value)) => value.rightIor
                  case pj => mkErrorResult(s"Grouped result of wrong shape: $pj")
                }
              }
          })

          gr.map(ProtoJson.fromValues)

        case (_, ListType(tpe)) =>
          cursor.asList.flatMap { cursors =>
            runList(query, tpe, cursors, Kleisli(_.rightIor))
          }

        case (_, NullableType(tpe)) =>
          cursor.asNullable.sequence.map { rc =>
            for {
              c     <- rc
              value <- runValue(query, tpe, c)
            } yield value
          }.getOrElse(ProtoJson.fromJson(Json.Null).rightIor)

        case (_, (_: ScalarType) | (_: EnumType)) =>
          cursor.asLeaf.map(ProtoJson.fromJson)

        case (_, (_: ObjectType) | (_: InterfaceType) | (_: UnionType)) =>
          runFields(query, tpe, cursor).map(ProtoJson.fromFields)

        case _ =>
          mkErrorResult(s"Stuck at type $tpe for ${query.render}")
      }
    }
}

object QueryInterpreter {
  /**
   * Opaque type of partially constructed query results.
   *
   * Values may be fully expanded Json values, objects or arrays which not
   * yet fully evaluated subtrees, or subqueries which are deferred to the
   * next stage or another component of a composite interpreter.
   */
  type ProtoJson <: AnyRef

  object ProtoJson {
    private[QueryInterpreter] sealed trait DeferredJson
    // A result which is deferred to the next stage or component of this interpreter.
    private[QueryInterpreter] case class StagedJson[F[_]](interpreter: QueryInterpreter[F], query: Query, rootTpe: Type) extends DeferredJson
    // A partially constructed object which has at least one deferred subtree.
    private[QueryInterpreter] case class ProtoObject(fields: List[(String, ProtoJson)])
    // A partially constructed array which has at least one deferred element.
    private[QueryInterpreter] case class ProtoArray(elems: List[ProtoJson])
    // A result which will yield a selection from its child
    private[QueryInterpreter] case class ProtoSelect(elem: ProtoJson, fieldName: String)

    /**
     * Delegate `query` to the interpreter `interpreter`. When evaluated by
     * that interpreter the query will have expected type `rootTpe`.
     */
    def staged[F[_]](interpreter: QueryInterpreter[F], query: Query, rootTpe: Type): ProtoJson =
      wrap(StagedJson(interpreter, query, rootTpe))

    def fromJson(value: Json): ProtoJson = wrap(value)

    /**
     * Combine possibly partial fields to create a possibly partial object.
     *
     * If all fields are complete then they will be combined as a complete
     * Json object.
     */
    def fromFields(fields: List[(String, ProtoJson)]): ProtoJson =
      if(fields.forall(_._2.isInstanceOf[Json]))
        wrap(Json.fromFields(fields.asInstanceOf[List[(String, Json)]]))
      else
        wrap(ProtoObject(fields))

    /**
     * Combine possibly partial values to create a possibly partial array.
     *
     * If all values are complete then they will be combined as a complete
     * Json array.
     */
    def fromValues(elems: List[ProtoJson]): ProtoJson =
      if(elems.forall(_.isInstanceOf[Json]))
        wrap(Json.fromValues(elems.asInstanceOf[List[Json]]))
      else
        wrap(ProtoArray(elems))

    /**
     * Select a value from a possibly partial object.
     *
     * If the object is complete the selection will be a complete
     * Json value.
     */
    def select(elem: ProtoJson, fieldName: String): ProtoJson =
      elem match {
        case j: Json =>
          wrap(j.asObject.flatMap(_(fieldName)).getOrElse(Json.Null))
        case _ =>
          wrap(ProtoSelect(elem, fieldName))
      }

    /**
     * Test whether the argument contains any deferred subtrees
     *
     * Yields `true` if the argument contains any component or staged
     * subtrees, false otherwise.
     */
    def isDeferred(p: ProtoJson): Boolean =
      p.isInstanceOf[DeferredJson]

    def mergeObjects(elems: List[ProtoJson]): ProtoJson = {
      def loop(elems: List[ProtoJson], acc: List[(String, ProtoJson)]): List[(String, ProtoJson)] = elems match {
        case Nil                       => acc
        case (j: Json) :: tl =>
          j.asObject match {
            case Some(obj)             => loop(tl, acc ++ obj.keys.zip(obj.values.map(fromJson)))
            case None                  => loop(tl, acc)
          }
        case ProtoObject(fields) :: tl => loop(tl, acc ++ fields)
        case _ :: tl                   => loop(tl, acc)
      }

      elems match {
        case Nil        => wrap(Json.Null)
        case hd :: Nil  => hd
        case _          =>
          loop(elems, Nil) match {
            case Nil    => wrap(Json.Null)
            case fields => fromFields(fields)
          }
      }
    }

    def unpackObject(p: ProtoJson): Option[List[ProtoJson]] =
      p match {
        case ProtoObject(List((_, packedElems))) => unpackList(packedElems)
        case j: Json if j.isObject =>
          j.asObject.flatMap(jo =>
            if (jo.size != 1) None
            else {
              val List((_, packedElems)) = jo.toList
              packedElems.asArray.map(v => wrapList(v.toList))
            }
          )
        case _ => None
      }

    def unpackList(p: ProtoJson): Option[List[ProtoJson]] =
      p match {
        case ProtoArray(elems) => Some(elems)
        case j: Json if j.isArray => j.asArray.map(ja => wrapList(ja.toList))
        case _ => None
      }

    private def wrap(j: AnyRef): ProtoJson = j.asInstanceOf[ProtoJson]
    private def wrapList(l: List[AnyRef]): List[ProtoJson] = l.asInstanceOf[List[ProtoJson]]
  }

  import ProtoJson._

  /**
   * Complete a possibly partial result.
   *
   * Completes a single possibly partial result as described for
   * `completeAll`.
   */
  def complete[F[_]: Monad](pj: ProtoJson): F[Result[Json]] =
    completeAll[F](List(pj)).map {
      case (errors, List(value)) =>
        NonEmptyChain.fromChain(errors) match {
          case Some(errors) => Ior.Both(errors, value)
          case None => value.rightIor
        }
    }

  /** Complete a collection of possibly deferred results.
   *
   *  Each result is completed by locating any subtrees which have been
   *  deferred or delegated to some other component interpreter in an
   *  overall composite interpreter. Deferred subtrees are gathered,
   *  grouped by their associated interpreter and then evaluated in
   *  batches. The results of these batch evaluations are then
   *  completed in a subsequent stage recursively until the results are
   *  fully evaluated or yield errors.
   *
   *  Complete results are substituted back into the corresponding
   *  enclosing Json.
   *
   *  Errors are aggregated across all the results and are accumulated
   *  on the `Left` of the result.
   */
  def completeAll[F[_]: Monad](pjs: List[ProtoJson]): F[(Chain[Json], List[Json])] = {
    def gatherDeferred(pj: ProtoJson): List[DeferredJson] = {
      @tailrec
      def loop(pending: Chain[ProtoJson], acc: List[DeferredJson]): List[DeferredJson] =
        pending.uncons match {
          case None => acc
          case Some((hd, tl)) => hd match {
            case _: Json              => loop(tl, acc)
            case d: DeferredJson      => loop(tl, d :: acc)
            case ProtoObject(fields)  => loop(Chain.fromSeq(fields.map(_._2)) ++ tl, acc)
            case ProtoArray(elems)    => loop(Chain.fromSeq(elems) ++ tl, acc)
            case ProtoSelect(elem, _) => loop(elem +: tl, acc)
          }
        }

      pj match {
        case _: Json => Nil
        case _ => loop(Chain.one(pj), Nil)
      }
    }

    def scatterResults(pj: ProtoJson, subst: mutable.Map[DeferredJson, Json]): Json = {
      def loop(pj: ProtoJson): Json =
        pj match {
          case p: Json         => p
          case d: DeferredJson => subst(d)
          case ProtoObject(fields) =>
            val newFields: List[(String, Json)] =
              fields.flatMap { case (label, pvalue) =>
                val value = loop(pvalue)
                if (isDeferred(pvalue) && value.isObject) {
                  value.asObject.get.toList match {
                    case List((_, value)) => List((label, value))
                    case other => other
                  }
                }
                else List((label, value))
              }
            Json.fromFields(newFields)

          case ProtoArray(elems) =>
            val elems0 = elems.map(loop)
            Json.fromValues(elems0)
          case ProtoSelect(elem, fieldName) =>
            loop(elem).asObject.flatMap(_(fieldName)).getOrElse(Json.Null)
        }

      loop(pj)
    }

    val collected = pjs.flatMap(gatherDeferred)

    val (good, bad, errors0) =
      collected.foldLeft((List.empty[(DeferredJson, QueryInterpreter[F], (Query, Type))], List.empty[DeferredJson], Chain.empty[Json])) {
        case ((good, bad, errors), d@StagedJson(interpreter, query, rootTpe)) =>
          ((d, interpreter.asInstanceOf[QueryInterpreter[F]], (query, rootTpe)) :: good, bad, errors)
      }

    val grouped = good.groupMap(_._2)(e => (e._1, e._3)).toList

    val staged =
      (grouped.traverse {
        case (i, dq) =>
          val (ds, qs) = dq.unzip
          for {
            pnext <- i.runRootValues(qs)
            next  <- completeAll[F](pnext._2)
          } yield (pnext._1 ++ next._1, ds.zip(next._2))
      }).map(Monoid.combineAll(_))

    staged.map {
      case (errors1, assoc) =>
        val subst = {
          val m = new java.util.IdentityHashMap[DeferredJson, Json]
          bad.foreach(dj => m.put(dj, Json.Null))
          assoc.foreach { case (d, j) => m.put(d, j) }
          m.asScala
        }
        val values = pjs.map(pj => scatterResults(pj, subst))
        (errors0 ++ errors1, values)
    }
  }

  /**
   * Construct a GraphQL response from the possibly absent result `data`
   * and a collection of errors.
   */
  def mkResponse(data: Option[Json], errors: List[Json]): Json = {
    val dataField = data.map { value => ("data", value) }.toList
    val fields =
      (dataField, errors) match {
        case (Nil, Nil)   => List(("errors", Json.fromValues(List(mkError("Invalid query")))))
        case (data, Nil)  => data
        case (data, errs) => ("errors", Json.fromValues(errs)) :: data
      }
    Json.fromFields(fields)
  }

  /** Construct a GraphQL response from a `Result`. */
  def mkResponse(result: Result[Json]): Json =
    mkResponse(result.right, result.left.map(_.toList).getOrElse(Nil))

  /**
   *  Construct a GraphQL error response from a `Result`, ignoring any
   *  right hand side in `result`.
   */
  def mkInvalidResponse(result: Result[Query]): Json =
    mkResponse(None, result.left.map(_.toList).getOrElse(Nil))

  /** Construct a GraphQL error object */
  def mkError(message: String, locations: List[(Int, Int)] = Nil, path: List[String] = Nil): Json = {
    val locationsField =
      if (locations.isEmpty) Nil
      else
        List((
          "locations",
          Json.fromValues(locations.map { case (line, col) => json""" { "line": $line, "col": $col } """ })
        ))
    val pathField =
      if (path.isEmpty) Nil
      else List(("path", Json.fromValues(path.map(Json.fromString))))

    Json.fromFields(("message", Json.fromString(message)) :: locationsField ++ pathField)
  }

  def mkOneError(message: String, locations: List[(Int, Int)] = Nil, path: List[String] = Nil): NonEmptyChain[Json] =
    NonEmptyChain.one(mkError(message, locations, path))

  /** Construct a GraphQL error object as the left hand side of a `Result` */
  def mkErrorResult[T](message: String, locations: List[(Int, Int)] = Nil, path: List[String] = Nil): Result[T] =
    Ior.leftNec(mkError(message, locations, path))
}
