// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
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
package generic

import scala.annotation.{nowarn, tailrec}

import cats.implicits._
import shapeless.{Coproduct, Generic, ::, HList, HNil, Inl, Inr, LabelledGeneric}
import shapeless.ops.hlist.{LiftAll => PLiftAll}
import shapeless.ops.coproduct.{LiftAll => CLiftAll}
import shapeless.ops.record.Keys

import syntax._
import Cursor.AbstractCursor
import ShapelessUtils._

trait ScalaVersionSpecificGenericMappingLike[F[_]] extends Mapping[F] { self: GenericMappingLike[F] =>
  trait MkObjectCursorBuilder[T] {
    def apply(tpe: Type): ObjectCursorBuilder[T]
  }

  object MkObjectCursorBuilder {
    type FieldMap[T] = Map[String, (Context, T, Option[Cursor], Env) => Result[Cursor]]

    implicit def productCursorBuilder[T <: Product, R <: HList, L <: HList]
      (implicit
        @nowarn gen: Generic.Aux[T, R],
        elems0: => PLiftAll[CursorBuilder, R],
        @nowarn lgen: LabelledGeneric.Aux[T, L],
        keys0: Keys[L]
      ): MkObjectCursorBuilder[T] =
      new MkObjectCursorBuilder[T] {
        def apply(tpe: Type): ObjectCursorBuilder[T] = {
          def fieldMap: Map[String, (Context, T, Option[Cursor], Env) => Result[Cursor]] = {
            val keys: List[String] = unsafeToList[Symbol](keys0()).map(_.name)
            val elems = unsafeToList[CursorBuilder[Any]](elems0.instances)
            keys.zip(elems.zipWithIndex).map {
              case (fieldName, (elem, idx)) =>
                def build(context: Context, focus: T, parent: Option[Cursor], env: Env) =
                  elem.build(context, focus.productElement(idx), parent, env)
                (fieldName, build _)
            }.toMap
          }
          new Impl[T](tpe, fieldMap)
        }
      }

    class Impl[T](tpe0: Type, fieldMap0: => FieldMap[T]) extends ObjectCursorBuilder[T] {
      lazy val fieldMap = fieldMap0

      val tpe = tpe0

      def build(context: Context, focus: T, parent: Option[Cursor], env: Env): Result[Cursor] =
        CursorImpl(context.asType(tpe), focus, fieldMap, parent, env).success

      def renameField(from: String, to: String): ObjectCursorBuilder[T] =
        transformFieldNames { case `from` => to ; case other => other }
      def transformFieldNames(f: String => String): ObjectCursorBuilder[T] =
        new Impl(tpe, fieldMap0.map { case (k, v) => (f(k), v) })
      def transformField[U](fieldName: String)(f: T => Result[U])(implicit cb: => CursorBuilder[U]): ObjectCursorBuilder[T] = {
        def build(context: Context, focus: T, parent: Option[Cursor], env: Env) =
          f(focus).flatMap(f => cb.build(context, f, parent, env))

        new Impl(tpe, fieldMap0.updated(fieldName, build _))
      }
    }

    case class CursorImpl[T](context: Context, focus: T, fieldMap: FieldMap[T], parent: Option[Cursor], env: Env)
      extends AbstractCursor {
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

      override def hasField(fieldName: String): Boolean =
        fieldMap.contains(fieldName) || fieldMapping(context, fieldName).isDefined

      override def field(fieldName: String, resultName: Option[String]): Result[Cursor] = {
        val localField =
          fieldMap.get(fieldName).toResult(s"No field '$fieldName' for type $tpe").flatMap { f =>
            f(context.forFieldOrAttribute(fieldName, resultName), focus, Some(this), Env.empty)
          }

        localField orElse mkCursorForField(this, fieldName, resultName)
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
      def build(context: Context, focus: T, parent: Option[Cursor], env: Env): Result[Cursor] = {
        val builder = elems(sel(focus))
        builder.build(context.asType(tpe), focus, parent, env).map(cursor => CursorImpl(tpe, builder.tpe, cursor, parent, env))
      }
    }

    case class CursorImpl[T](tpe0: Type, rtpe: Type, cursor: Cursor, parent: Option[Cursor], env: Env)
      extends AbstractCursor {
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

      def focus: Any = cursor.focus
      val context: Context = cursor.context.asType(tpe0)

      override def hasField(fieldName: String): Boolean = cursor.hasField(fieldName)

      override def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
        cursor.field(fieldName, resultName) orElse mkCursorForField(this, fieldName, resultName)

      override def narrowsTo(subtpe: TypeRef): Boolean =
        subtpe <:< tpe && rtpe <:< subtpe

      override def narrow(subtpe: TypeRef): Result[Cursor] =
        if (narrowsTo(subtpe)) copy(tpe0 = subtpe).success
        else Result.internalError(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")
    }
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
