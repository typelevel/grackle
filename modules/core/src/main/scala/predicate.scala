// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import scala.annotation.tailrec
import scala.util.matching.Regex

import cats.Apply
import cats.data.Ior
import cats.kernel.{ Eq, Order }
import cats.implicits._

import Query._
import QueryInterpreter.mkErrorResult

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
    def apply(c: Cursor): Result[Boolean] = x(c).map(y.contains_)

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
