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
package circe

import scala.collection.Factory

import cats.MonadThrow
import cats.implicits._
import fs2.Stream
import io.circe.Json
import io.circe.Encoder
import org.tpolecat.sourcepos.SourcePos

import syntax._
import Cursor.DeferredCursor
import ScalarType._

abstract class CirceMapping[F[_]](implicit val M: MonadThrow[F]) extends Mapping[F] with CirceMappingLike[F]

trait CirceMappingLike[F[_]] extends Mapping[F] {

  // Syntax to allow Circe-specific root effects
  implicit class CirceMappingRootEffectSyntax(self: RootEffect.type) {
    def computeJson(fieldName: String)(effect: (Path, Env) => F[Result[Json]])(implicit pos: SourcePos): RootEffect =
      self.computeCursor(fieldName)((p, e) => effect(p, e).map(_.map(circeCursor(p, e, _))))

    def computeEncodable[A](fieldName: String)(effect: (Path, Env) => F[Result[A]])(implicit pos: SourcePos, enc: Encoder[A]): RootEffect =
      computeJson(fieldName)((p, e) => effect(p, e).map(_.map(enc(_))))
  }

  implicit class CirceMappingRootStreamSyntax(self: RootStream.type) {
    def computeJson(fieldName: String)(effect: (Path, Env) => Stream[F, Result[Json]])(implicit pos: SourcePos): RootStream =
      self.computeCursor(fieldName)((p, e) => effect(p, e).map(_.map(circeCursor(p, e, _))))

    def computeEncodable[A](fieldName: String)(effect: (Path, Env) => Stream[F, Result[A]])(implicit pos: SourcePos, enc: Encoder[A]): RootStream =
      computeJson(fieldName)((p, e) => effect(p, e).map(_.map(enc(_))))
  }

  def circeCursor(path: Path, env: Env, value: Json): Cursor =
    if(path.isRoot)
      CirceCursor(Context(path.rootTpe), value, None, env)
    else
      DeferredCursor(path, (context, parent) => CirceCursor(context, value, Some(parent), env).success)

  override def mkCursorForMappedField(parent: Cursor, fieldContext: Context, fm: FieldMapping): Result[Cursor] =
    (fm, parent.focus) match {
      case (CirceField(_, json, _), _) =>
        CirceCursor(fieldContext, json, Some(parent), parent.env).success
      case (CursorFieldJson(_, f, _, _), _) =>
        f(parent).map(res => CirceCursor(fieldContext, focus = res, parent = Some(parent), env = parent.env))
      case _ =>
        super.mkCursorForMappedField(parent, fieldContext, fm)
    }

  sealed trait CirceFieldMapping extends FieldMapping {
    def subtree: Boolean = true
  }

  case class CirceField(fieldName: String, value: Json, hidden: Boolean = false)(implicit val pos: SourcePos) extends CirceFieldMapping

  case class CursorFieldJson(fieldName: String, f: Cursor => Result[Json], required: List[String], hidden: Boolean = false)(
    implicit val pos: SourcePos
  ) extends CirceFieldMapping

  case class CirceCursor(
    context: Context,
    focus:  Json,
    parent: Option[Cursor],
    env:    Env
  ) extends Cursor {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def mkChild(context: Context = context, focus: Json = focus): CirceCursor =
      CirceCursor(context, focus, Some(this), Env.empty)

    def isLeaf: Boolean =
      tpe.dealias match {
        case BooleanType => focus.isBoolean
        case StringType|IDType => focus.isString
        case IntType|FloatType => focus.isNumber
        case _: EnumType => focus.isString
        case _ => false
      }

    def asLeaf: Result[Json] =
      tpe.dealias match {
        case BooleanType       if focus.isBoolean => focus.success
        case StringType|IDType if focus.isString  => focus.success
        case IntType           if focus.isNumber  =>
          focus.asNumber.flatMap(_.toLong.map(Json.fromLong))
            .toResultOrError(s"Expected Int found ${focus.noSpaces}")
        case FloatType         if focus.isNumber  => focus.success
        case e: EnumType       if focus.isString  =>
          if (focus.asString.exists(e.hasValue)) focus.success
          else Result.internalError(s"Expected Enum ${e.name}, found ${focus.noSpaces}")
        case _: ScalarType     if !focus.isObject => focus.success // custom Scalar; any non-object type is fine
        case _ =>
          Result.internalError(s"Expected Scalar type, found $tpe for focus ${focus.noSpaces} at ${context.path.reverse.mkString("/")} ")
      }

    def preunique: Result[Cursor] = {
      val listTpe = tpe.nonNull.list
      if(focus.isArray)
        mkChild(context.asType(listTpe), focus).success
      else
        Result.internalError(s"Expected List type, found $focus for ${listTpe}")
    }

    def isList: Boolean = tpe.isList && focus.isArray

    def asList[C](factory: Factory[Cursor, C]): Result[C] = tpe match {
      case ListType(elemTpe) if focus.isArray =>
        focus.asArray.map(_.view.map(e => mkChild(context.asType(elemTpe), e)).to(factory))
          .toResultOrError(s"Expected List type, found $tpe for focus ${focus.noSpaces}")
      case _ =>
        Result.internalError(s"Expected List type, found $tpe for focus ${focus.noSpaces}")
    }

    def listSize: Result[Int] = tpe match {
      case ListType(_) if focus.isArray =>
        focus.asArray.map(_.size)
          .toResultOrError(s"Expected List type, found $tpe for focus ${focus.noSpaces}")
      case _ =>
        Result.internalError(s"Expected List type, found $tpe for focus ${focus.noSpaces}")
    }

    def isNullable: Boolean = tpe.isNullable

    def asNullable: Result[Option[Cursor]] = tpe match {
      case NullableType(tpe) =>
        if (focus.isNull) None.success
        else Some(mkChild(context.asType(tpe))).success
      case _ => Result.internalError(s"Expected Nullable type, found $focus for $tpe")
    }

    def isDefined: Result[Boolean] = tpe match {
      case NullableType(_) => (!focus.isNull).success
      case _ => Result.internalError(s"Expected Nullable type, found $focus for $tpe")
    }

    def narrowsTo(subtpe: TypeRef): Result[Boolean] =
      (subtpe <:< tpe &&
        ((subtpe.dealias, focus.asObject) match {
          case (nt: TypeWithFields, Some(obj)) =>
            nt.fields.forall { f =>
              f.tpe.isNullable || obj.contains(f.name)
            } && obj.keys.forall(nt.hasField)

          case _ => false
        })).success

    def narrow(subtpe: TypeRef): Result[Cursor] =
      narrowsTo(subtpe).flatMap { n =>
        if (n)
          mkChild(context.asType(subtpe)).success
        else
          Result.internalError(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")
      }

    def field(fieldName: String, resultName: Option[String]): Result[Cursor] = {
      val localField =
        for {
          obj <- focus.asObject
          f   <- obj(fieldName)
        } yield mkChild(context.forFieldOrAttribute(fieldName, resultName), f)

      localField.map(_.success).getOrElse(mkCursorForField(this, fieldName, resultName))
    }
  }
}
