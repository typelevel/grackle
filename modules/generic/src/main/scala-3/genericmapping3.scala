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

import cats.implicits._
import shapeless3.deriving._

import syntax._
import Cursor.AbstractCursor

trait ScalaVersionSpecificGenericMappingLike[F[_]] extends Mapping[F] { self: GenericMappingLike[F] =>
  trait MkObjectCursorBuilder[T] {
    def apply(tpe: Type): ObjectCursorBuilder[T]
  }

  object MkObjectCursorBuilder {
    type FieldMap[T] = Map[String, (Context, T, Option[Cursor], Env) => Result[Cursor]]

    implicit def productCursorBuilder[T <: Product]
      (implicit
        inst:      K0.ProductInstances[CursorBuilder, T],
        labelling: Labelling[T],
      ): MkObjectCursorBuilder[T] =
      new MkObjectCursorBuilder[T] {
        def apply(tpe: Type): ObjectCursorBuilder[T] = {
          def fieldMap: FieldMap[T] = {
            labelling.elemLabels.zipWithIndex.map {
              case (fieldName, idx) =>
                def build(context: Context, focus: T, parent: Option[Cursor], env: Env) =
                  inst.project(focus)(idx)([t] => (builder: CursorBuilder[t], pt: t) => builder.build(context, pt, parent, env))
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
    implicit def coproductCursorBuilder[T]
      (implicit
        inst: => K0.CoproductInstances[CursorBuilder, T]
      ): MkInterfaceCursorBuilder[T] =
      new MkInterfaceCursorBuilder[T] {
        def apply(tpe: Type): CursorBuilder[T] = new Impl[T](tpe, inst)
      }

    class Impl[T](tpe0: Type, inst: K0.CoproductInstances[CursorBuilder, T]) extends CursorBuilder[T] {
      val tpe = tpe0
      def build(context: Context, focus: T, parent: Option[Cursor], env: Env): Result[Cursor] = {
        inst.fold(focus)([t] => (builder: CursorBuilder[t], pt: t) =>
          builder.build(context, pt, parent, env).map(cursor => CursorImpl(tpe, builder.tpe, cursor, parent, env))
        )
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
