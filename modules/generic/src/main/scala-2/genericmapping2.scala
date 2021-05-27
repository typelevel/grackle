// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import scala.annotation.{ nowarn, tailrec }

import cats.implicits._
import shapeless.{ Coproduct, Generic, ::, HList, HNil, Inl, Inr, LabelledGeneric }
import shapeless.ops.hlist.{ LiftAll => PLiftAll }
import shapeless.ops.coproduct.{ LiftAll => CLiftAll }
import shapeless.ops.record.Keys

import Cursor.Env
import QueryInterpreter.{ mkErrorResult, mkOneError }
import ShapelessUtils._

trait MkObjectCursorBuilder[T] {
  def apply(tpe: Type): ObjectCursorBuilder[T]
}

object MkObjectCursorBuilder {
  implicit def productCursorBuilder[T <: Product, R <: HList, L <: HList]
    (implicit
      @nowarn gen: Generic.Aux[T, R],
      elems0: => PLiftAll[CursorBuilder, R],
      @nowarn lgen: LabelledGeneric.Aux[T, L],
      keys0: Keys[L]
    ): MkObjectCursorBuilder[T] =
    new MkObjectCursorBuilder[T] {
      def apply(tpe: Type): ObjectCursorBuilder[T] = {
        def fieldMap: Map[String, (List[String], T, Option[Cursor], Env) => Result[Cursor]] = {
          val keys: List[String] = unsafeToList[Symbol](keys0()).map(_.name)
          val elems = unsafeToList[CursorBuilder[Any]](elems0.instances)
          keys.zip(elems.zipWithIndex).map {
            case (fieldName, (elem, idx)) => (fieldName, (p: List[String], t: T, c: Option[Cursor], e: Env) => elem.build(p, t.productElement(idx), c, e))
          }.toMap
        }
        new Impl[T](tpe, fieldMap)
      }
    }

  class Impl[T](tpe0: Type, fieldMap0: => Map[String, (List[String], T, Option[Cursor], Env) => Result[Cursor]]) extends ObjectCursorBuilder[T] {
    lazy val fieldMap = fieldMap0

    val tpe = tpe0

    def build(path: List[String], focus: T, parent: Option[Cursor], env: Env): Result[Cursor] =
      CursorImpl(path, tpe, focus, fieldMap, parent, env).rightIor

    def renameField(from: String, to: String): ObjectCursorBuilder[T] =
      transformFieldNames { case `from` => to ; case other => other }
    def transformFieldNames(f: String => String): ObjectCursorBuilder[T] =
      new Impl(tpe, fieldMap0.map { case (k, v) => (f(k), v) })
    def transformField[U](fieldName: String)(f: T => Result[U])(implicit cb: => CursorBuilder[U]): ObjectCursorBuilder[T] =
      new Impl(tpe, fieldMap0.updated(fieldName, (path: List[String], focus: T, parent: Option[Cursor], env: Env) => f(focus).flatMap(f => cb.build(path, f, parent, env))))
  }

  case class CursorImpl[T](path: List[String], tpe: Type, focus: T, fieldMap: Map[String, (List[String], T, Option[Cursor], Env) => Result[Cursor]], parent: Option[Cursor], env: Env)
    extends AbstractCursor[Product] {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    override def hasField(fieldName: String): Boolean = fieldMap.contains(fieldName)

    override def field(fieldName: String): Result[Cursor] = {
      fieldMap.get(fieldName).toRightIor(mkOneError(s"No field '$fieldName' for type $tpe")).flatMap(f => f(fieldName :: path, focus, Some(this), Env.empty))
    }
  }
}

trait MkInterfaceCursorBuilder[T] {
  def apply(tpe: Type): CursorBuilder[T]
}

object MkInterfaceCursorBuilder {
  implicit def coproductCursorBuilder[T, R <: Coproduct]
    (implicit
      gen: Generic.Aux[T, R],
      elems0: => CLiftAll[CursorBuilder, R]
    ): MkInterfaceCursorBuilder[T] =
    new MkInterfaceCursorBuilder[T] {
      def apply(tpe: Type): CursorBuilder[T] = {
        def elems = unsafeToList[CursorBuilder[T]](elems0.instances).toArray
        def sel(t: T) = {
          @tailrec
          def loop(c: Coproduct, acc: Int): Int = c match {
            case Inl(_) => acc
            case Inr(tl) => loop(tl, acc+1)
          }
          loop(gen.to(t), 0)
        }
        new Impl[T](tpe, sel, elems)
      }
    }

  class Impl[T](tpe0: Type, sel: T => Int, elems: => Array[CursorBuilder[T]]) extends CursorBuilder[T] {
    val tpe = tpe0
    def build(path: List[String], focus: T, parent: Option[Cursor], env: Env): Result[Cursor] = {
      val builder = elems(sel(focus))
      builder.build(path, focus, parent, env).map(cursor => CursorImpl(tpe, builder.tpe, cursor, parent, env))
    }
  }

  case class CursorImpl[T](tpe: Type, rtpe: Type, cursor: Cursor, parent: Option[Cursor], env: Env)
    extends AbstractCursor[T] {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def focus: Any = cursor.focus
    def path: List[String] = cursor.path

    override def hasField(fieldName: String): Boolean = cursor.hasField(fieldName)

    override def field(fieldName: String): Result[Cursor] = cursor.field(fieldName)

    override def narrowsTo(subtpe: TypeRef): Boolean =
      subtpe <:< tpe && rtpe <:< subtpe

    override def narrow(subtpe: TypeRef): Result[Cursor] =
      if (narrowsTo(subtpe)) copy(tpe = subtpe).rightIor
      else mkErrorResult(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")
  }
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
