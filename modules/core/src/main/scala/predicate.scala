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

import scala.annotation.tailrec
import scala.util.matching.Regex

import cats.Apply
import cats.kernel.{ Eq, Order }
import cats.implicits._

import syntax._

/**
 * A reified function over a `Cursor`.
 *
 * Query interpreters will typically need to introspect predicates (eg. in the doobie module
 * we need to be able to construct where clauses from predicates over fields/attributes), so
 * these cannot be arbitrary functions `Cursor => Boolean`.
 */
trait Term[T] extends Product with Serializable { // fun fact: making this covariant crashes Scala 3
  def apply(c: Cursor): Result[T]

  def children: List[Term[_]]

  def fold[Acc](acc: Acc)(f: (Acc, Term[_]) => Acc): Acc =
    children.foldLeft(f(acc, this)) { case (acc, child) => child.fold(acc)(f) }

  def exists(f: Term[_] => Boolean): Boolean =
    f(this) || children.exists(_.exists(f))

  def forall(f: Term[_] => Boolean): Boolean =
    f(this) && children.forall(_.forall(f))

  def forallR(f: Term[_] => Result[Boolean]): Result[Boolean] =
    f(this).flatMap { p =>
      if (!p) false.success
      else children.foldLeftM(true) { case (acc, elem) => if(!acc) false.success else elem.forallR(f) }
    }
}

object Term extends TermLow {
  implicit def path2Term[A](p: Path): Term[A] =
    p.asTerm[A]
}

trait TermLow {
  implicit def path2ListTerm[A](p: Path): Term[List[A]] =
    p.asTerm[List[A]]
}

trait Path {
  def rootTpe: Type
  def path: List[String]
  def tpe: Type
  def asTerm[A]: Term[A]

  def /(elem: String): Path
  def %(ntpe: NamedType): Path

  def isRoot = path.isEmpty
}

/** A path starting from some root type. */
object Path {
  /** Construct an empty `Path` rooted at the given type. */
  def from(tpe: Type): Path =
    PathImpl(tpe, Nil, tpe)

  case class PathImpl(rootTpe: Type, path: List[String], tpe: Type) extends Path {
    def asTerm[A]: Term[A] =
      if (isList) PathTerm.ListPath(path).asInstanceOf[Term[A]]
      else PathTerm.UniquePath(path)

    lazy val isList: Boolean = rootTpe.pathIsList(path)

    def /(elem: String): Path = {
      val ftpe = tpe.underlyingField(elem).getOrElse(ScalarType.AttributeType) // Matches Context#forFieldOrAttribute
      copy(path = path :+ elem, tpe = ftpe)
    }

    def %(ntpe: NamedType): Path =
      if(ntpe <:< tpe) copy(tpe = ntpe)
      else throw new IllegalArgumentException(s"Type $ntpe is not a subtype of $tpe")
  }
}

sealed trait PathTerm {
  def children: List[Term[_]] =  Nil
  def path: List[String]
}
object PathTerm {

  case class ListPath[A](path: List[String]) extends Term[List[A]] with PathTerm {
    def apply(c: Cursor): Result[List[A]] =
      c.flatListPath(path).flatMap(_.traverse {
        case Predicate.ScalarFocus(f) => f.success
        case _ => Result.internalError("impossible")
      }).asInstanceOf[Result[List[A]]]
  }

  case class UniquePath[A](path: List[String]) extends Term[A] with PathTerm {
    def apply(c: Cursor): Result[A] =
      c.listPath(path).flatMap {
        case List(Predicate.ScalarFocus(a: A @unchecked)) => a.success
        case other => Result.internalError(s"Expected exactly one element for path $path found $other")
      }
  }

}

trait Predicate extends Term[Boolean]

object Predicate {
  object ScalarFocus {
    def unapply(c: Cursor): Option[Any] =
      if (c.isLeaf) Some(c.focus)
      else if (c.isNullable && c.tpe.nonNull.isLeaf)
        c.asNullable.toOption.map(_.flatMap(c => unapply(c)))
      else None
  }

  case object True extends Predicate {
    def apply(c: Cursor): Result[Boolean] = true.success

    def children = Nil
  }

  case object False extends Predicate {
    def apply(c: Cursor): Result[Boolean] = false.success

    def children = Nil
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
    def apply(c: Cursor): Result[T] = v.success
    def children = Nil
  }

  case class And(x: Predicate, y: Predicate) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_ && _)
    def children = List(x, y)
  }

  object And {
    def combineAll(preds: List[Predicate]): Predicate =
      preds match {
        case Nil => True
        case List(pred) => pred
        case hd :: tail => tail.foldRight(hd)((elem, acc) => And(elem, acc))
      }
  }

  case class Or(x: Predicate, y: Predicate) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_ || _)
    def children = List(x, y)
  }

  case class Not(x: Predicate) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = x(c).map(! _)
    def children = List(x)
  }

  case class Eql[T: Eq](x: Term[T], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_ === _)
    def eqInstance: Eq[T] = implicitly[Eq[T]]
    def children = List(x, y)
    def subst(x: Term[T], y: Term[T]): Eql[T] = copy(x = x, y = y)
  }

  case class NEql[T: Eq](x: Term[T], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_ =!= _)
    def children = List(x, y)
    def subst(x: Term[T], y: Term[T]): NEql[T] = copy(x = x, y = y)
  }

  case class Contains[T: Eq](x: Term[List[T]], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))((xs, y0) => xs.exists(_ === y0))
    def children = List(x, y)
    def subst(x: Term[List[T]], y: Term[T]): Contains[T] = copy(x = x, y = y)
  }

  case class Lt[T: Order](x: Term[T], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_.compare(_) < 0)
    def children = List(x, y)
    def subst(x: Term[T], y: Term[T]): Lt[T] = copy(x = x, y = y)
  }

  case class LtEql[T: Order](x: Term[T], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_.compare(_) <= 0)
    def children = List(x, y)
    def subst(x: Term[T], y: Term[T]): LtEql[T] = copy(x = x, y = y)
  }

  case class Gt[T: Order](x: Term[T], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_.compare(_) > 0)
    def children = List(x, y)
    def subst(x: Term[T], y: Term[T]): Gt[T] = copy(x = x, y = y)
  }

  case class GtEql[T: Order](x: Term[T], y: Term[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = Apply[Result].map2(x(c), y(c))(_.compare(_) >= 0)
    def children = List(x, y)
    def subst(x: Term[T], y: Term[T]): GtEql[T] = copy(x = x, y = y)
  }

  case class IsNull[T](x: Term[Option[T]], isNull: Boolean) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = x(c).map(_.isEmpty == isNull)
    def children = List(x)
  }

  case class In[T: Eq](x: Term[T], y: List[T]) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = x(c).map(y.contains_(_))
    def children = List(x)
    def subst(x: Term[T]): In[T] = copy(x = x)
  }

  object In {
    def fromEqls[T](eqls: List[Eql[T]]): Option[In[T]] = {
      val paths = eqls.map(_.x).distinct
      val consts = eqls.collect { case Eql(_, Const(c)) => c }
      if (eqls.map(_.x).distinct.sizeCompare(1) == 0 && consts.sizeCompare(eqls) == 0)
        Some(In(paths.head, consts)(eqls.head.eqInstance))
      else
        None
    }
  }

  case class AndB(x: Term[Int], y: Term[Int]) extends Term[Int] {
    def apply(c: Cursor): Result[Int] = Apply[Result].map2(x(c), y(c))(_ & _)
    def children = List(x, y)
  }

  case class OrB(x: Term[Int], y: Term[Int]) extends Term[Int] {
    def apply(c: Cursor): Result[Int] = Apply[Result].map2(x(c), y(c))(_ | _)
    def children = List(x, y)
  }

  case class XorB(x: Term[Int], y: Term[Int]) extends Term[Int] {
    def apply(c: Cursor): Result[Int] = Apply[Result].map2(x(c), y(c))(_ ^ _)
    def children = List(x, y)
  }

  case class NotB(x: Term[Int]) extends Term[Int] {
    def apply(c: Cursor): Result[Int] = x(c).map(~ _)
    def children = List(x)
  }

  case class Matches(x: Term[String], r: Regex) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = x(c).map(r.matches(_))
    def children = List(x)
  }

  case class StartsWith(x: Term[String], prefix: String) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = x(c).map(_.startsWith(prefix))
    def children = List(x)
  }

  case class ToUpperCase(x: Term[String]) extends Term[String] {
    def apply(c: Cursor): Result[String] = x(c).map(_.toUpperCase)
    def children = List(x)
  }

  case class ToLowerCase(x: Term[String]) extends Term[String] {
    def apply(c: Cursor): Result[String] = x(c).map(_.toLowerCase)
    def children = List(x)
  }
}
