// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import scala.annotation.tailrec

import cats.kernel.{ Eq, Order }
import cats.implicits._

import Cursor.Env
import Query._

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

  case class Environment(env: Env, child: Query) extends Query {
    def render = s"<environment: $env ${child.render}>"
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

  case class OrderSelection[T: Order](term: Term[T], ascending: Boolean = true, nullsLast: Boolean = true) {
    def apply(x: Cursor, y: Cursor): Int = {
      def deref(c: Cursor): Option[T] =
        if (c.isNullable) c.asNullable.getOrElse(None).flatMap(term(_).toOption)
        else term(c).toOption

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
        (c, deref(c).map { t => ds.indexWhere(_ === t) }.getOrElse(-1))
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

  case object Skipped extends Query {
    def render = "<skipped>"
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
  type Vars = Map[String, (Type, Value)]

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
