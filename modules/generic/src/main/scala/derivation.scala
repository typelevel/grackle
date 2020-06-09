// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import scala.annotation.tailrec

import cats.Monad
import cats.implicits._
import shapeless.{ Coproduct, Generic, ::, HList, HNil, Inl, Inr, LabelledGeneric, Typeable }
import shapeless.ops.hlist.{ LiftAll => PLiftAll }
import shapeless.ops.coproduct.{ LiftAll => CLiftAll }
import shapeless.ops.record.Keys

import Query.{ PossiblyRenamedSelect, Select, Wrap }
import QueryInterpreter.{ mkErrorResult, mkOneError, ProtoJson }
import ShapelessUtils._

object semiauto {
  final def deriveObjectCursorBuilder[T](implicit builder: => DerivedObjectCursorBuilder[T]): DerivedObjectCursorBuilder[T] = builder
  final def deriveInterfaceCursorBuilder[T](implicit builder: DerivedInterfaceCursorBuilder[T]): DerivedInterfaceCursorBuilder[T] = builder
}

trait ObjectCursorBuilder[T] extends CursorBuilder[T] {
  def ref(nme: String): CursorBuilder[T]
  def renamedField(from: String, to: String): CursorBuilder[T]
  def transformFieldNames(f: String => String): CursorBuilder[T]
  def transformField[U](fieldName: String)(f: T => Result[U])(implicit cb: => CursorBuilder[U]): CursorBuilder[T]
}

trait DerivedObjectCursorBuilder[T] extends ObjectCursorBuilder[T]

object DerivedObjectCursorBuilder {
  def apply[T](implicit cb: DerivedObjectCursorBuilder[T]): DerivedObjectCursorBuilder[T] = cb

  implicit def productCursorBuilder[T <: Product, R <: HList, L <: HList]
    (implicit
      gen: Generic.Aux[T, R],
      elems0: => PLiftAll[CursorBuilder, R],
      lgen: LabelledGeneric.Aux[T, L],
      keys0: Keys[L],
      tp: Typeable[T]
    ): DerivedObjectCursorBuilder[T] = {
      identity(gen)  // unused implicit warning without these
      identity(lgen) // remove when @nowarn lands in 2.13.2

      def fieldMap = {
        val keys: List[String] = unsafeToList[Symbol](keys0()).map(_.name)
        val elems = unsafeToList[CursorBuilder[Any]](elems0.instances)
        keys.zip(elems.zipWithIndex).map {
          case (fieldName, (elem, idx)) =>
            (fieldName, (t: T, tpe: Type) => elem.build(t.productElement(idx), tpe.field(fieldName)))
        }.toMap
      }
      new Impl[T](tp.describe, fieldMap)
    }

  class Impl[T](nme: String, fieldMap0: => Map[String, (T, Type) => Result[Cursor]]) extends DerivedObjectCursorBuilder[T] {
    lazy val fieldMap = fieldMap0

    def build(focus: T, tpe: Type): Result[Cursor] =
      CursorImpl(nme, fieldMap, focus, tpe).rightIor

    def ref(nme0: String): CursorBuilder[T] =
      new Impl(nme0, fieldMap0)
    def renamedField(from: String, to: String): CursorBuilder[T] =
      transformFieldNames { case `from` => to ; case other => other }
    def transformFieldNames(f: String => String): CursorBuilder[T] =
      new Impl(nme, fieldMap0.map { case (k, v) => (f(k), v) })
    def transformField[U](fieldName: String)(f: T => Result[U])(implicit cb: => CursorBuilder[U]): CursorBuilder[T] =
      new Impl(nme, fieldMap0.updated(fieldName, (focus: T, tpe: Type) => f(focus).flatMap(f => cb.build(f, tpe))))
  }

  case class CursorImpl[T](nme: String, fieldMap: Map[String, (T, Type) => Result[Cursor]], focus: T, tpe: Type)
    extends AbstractCursor[T] {
    override def hasField(fieldName: String): Boolean = fieldMap.contains(fieldName)

    override def field(fieldName: String): Result[Cursor] = {
      fieldMap.get(fieldName).toRightIor(mkOneError(s"No field '$fieldName' for type $tpe")).flatMap(f => f(focus, tpe.field(fieldName)))
    }

    override def narrowsTo(subtpe: TypeRef): Boolean =
      subtpe <:< tpe && subtpe.name == nme

    override def narrow(subtpe: TypeRef): Result[Cursor] =
      if (narrowsTo(subtpe)) copy(tpe = subtpe).rightIor
      else mkErrorResult(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")
  }
}

trait DerivedInterfaceCursorBuilder[T] extends CursorBuilder[T]

object DerivedInterfaceCursorBuilder {
  implicit def coproductCursorBuilder[T, R <: Coproduct]
    (implicit
      gen: Generic.Aux[T, R],
      elems0: => CLiftAll[CursorBuilder, R]
    ): DerivedInterfaceCursorBuilder[T] = {
      def elems = unsafeToList[CursorBuilder[T]](elems0.instances).toArray
      def sel(t: T) = {
        @tailrec
        def loop(c: Coproduct, acc: Int): Int = c match {
          case Inl(_) => acc
          case Inr(tl) => loop(tl, acc+1)
        }
        loop(gen.to(t), 0)
      }
      new Impl[T](sel, elems)
    }

  class Impl[T](sel: T => Int, elems: => Array[CursorBuilder[T]])  extends DerivedInterfaceCursorBuilder[T] {
    def build(focus: T, tpe: Type): Result[Cursor] =
      elems(sel(focus)).build(focus, tpe)
  }
}

class GenericQueryInterpreter[F[_]: Monad](root: PartialFunction[String, Result[Cursor]]) extends QueryInterpreter[F] {
  def runRootValue(query: Query, rootTpe: Type): F[Result[ProtoJson]] =
    (query match {
      case PossiblyRenamedSelect(Select(fieldName, _, child), resultName) =>
        if (root.isDefinedAt(fieldName))
          root(fieldName).flatMap(cursor => runValue(Wrap(resultName, child), rootTpe.field(fieldName), cursor))
        else
          mkErrorResult(s"No root field '$fieldName'")
      case _ =>
        mkErrorResult(s"Bad root query '${query.render}' in DataTypeQueryInterpreter")
    }).pure[F]
}

object ShapelessUtils {
  def unsafeToList[U](l: HList): List[U] = {
    @tailrec
    def loop(l: HList, acc: List[U]): List[U] = l match {
      case HNil => acc.reverse
      case hd :: tl => loop(tl, hd.asInstanceOf[U] :: acc)
    }
    loop(l, Nil)
  }
}
