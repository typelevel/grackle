// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import cats.implicits._
import shapeless3.deriving._

import Cursor.{AbstractCursor, Context, Env}
import QueryInterpreter.{mkErrorResult, mkOneError}

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
        CursorImpl(context.asType(tpe), focus, fieldMap, parent, env).rightIor

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

      override def hasField(fieldName: String): Boolean = fieldMap.contains(fieldName)

      override def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
        mkCursorForField(this, fieldName, resultName) orElse {
          fieldMap.get(fieldName).toRightIor(mkOneError(s"No field '$fieldName' for type $tpe")).flatMap { f =>
            f(context.forFieldOrAttribute(fieldName, resultName), focus, Some(this), Env.empty)
          }
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
        mkCursorForField(this, fieldName, resultName) orElse cursor.field(fieldName, resultName)

      override def narrowsTo(subtpe: TypeRef): Boolean =
        subtpe <:< tpe && rtpe <:< subtpe

      override def narrow(subtpe: TypeRef): Result[Cursor] =
        if (narrowsTo(subtpe)) copy(tpe0 = subtpe).rightIor
        else mkErrorResult(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")
    }
  }
}
