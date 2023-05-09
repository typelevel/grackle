// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package circe

import scala.collection.Factory

import cats.MonadThrow
import cats.implicits._
import fs2.Stream 
import io.circe.Json
import io.circe.Encoder
import org.tpolecat.sourcepos.SourcePos

import syntax._
import Cursor.{Context, DeferredCursor, Env}
import ScalarType._

abstract class CirceMapping[F[_]](implicit val M: MonadThrow[F]) extends Mapping[F] with CirceMappingLike[F]

trait CirceMappingLike[F[_]] extends Mapping[F] {

  // Syntax to allow Circe-specific root effects
  implicit class CirceMappingRootEffectSyntax(self: RootEffect.type) {
    def computeJson(fieldName: String)(effect: (Query, Path, Env) => F[Result[Json]])(implicit pos: SourcePos): RootEffect =
      self.computeCursor(fieldName)((q, p, e) => effect(q, p, e).map(_.map(circeCursor(p, e, _))))
    
    def computeEncodable[A](fieldName: String)(effect: (Query, Path, Env) => F[Result[A]])(implicit pos: SourcePos, enc: Encoder[A]): RootEffect =
      computeJson(fieldName)((q, p, e) => effect(q, p, e).map(_.map(enc(_))))
  }

  implicit class CirceMappingRootStreamSyntax(self: RootStream.type) {
    def computeJson(fieldName: String)(effect: (Query, Path, Env) => Stream[F, Result[Json]])(implicit pos: SourcePos): RootStream =
      self.computeCursor(fieldName)((q, p, e) => effect(q, p, e).map(_.map(circeCursor(p, e, _))))

    def computeEncodable[A](fieldName: String)(effect: (Query, Path, Env) => Stream[F, Result[A]])(implicit pos: SourcePos, enc: Encoder[A]): RootStream =
      computeJson(fieldName)((q, p, e) => effect(q, p, e).map(_.map(enc(_))))
  }

  def circeCursor(path: Path, env: Env, value: Json): Cursor =
    if(path.isRoot)
      CirceCursor(Context(path.rootTpe), value, None, env)
    else
      DeferredCursor(path, (context, parent) => CirceCursor(context, value, Some(parent), env).success)

  override def mkCursorForField(parent: Cursor, fieldName: String, resultName: Option[String]): Result[Cursor] = {
    val context = parent.context
    val fieldContext = context.forFieldOrAttribute(fieldName, resultName)
    (fieldMapping(context, fieldName), parent.focus) match {
      case (Some(CirceField(_, json, _)), _) =>
        CirceCursor(fieldContext, json, Some(parent), parent.env).success
      case (Some(CursorFieldJson(_, f, _, _)), _) =>
        f(parent).map(res => CirceCursor(fieldContext, focus = res, parent = Some(parent), env = parent.env))
      case (None|Some(_: EffectMapping), json: Json) =>
        val f = json.asObject.flatMap(_(fieldName))
        f match {
          case None if fieldContext.tpe.isNullable => CirceCursor(fieldContext, Json.Null, Some(parent), parent.env).success
          case Some(json) => CirceCursor(fieldContext, json, Some(parent), parent.env).success
          case _ =>
            Result.failure(s"No field '$fieldName' for type ${context.tpe}")
        }
      case _ =>
        super.mkCursorForField(parent, fieldName, resultName)
    }
  }

  sealed trait CirceFieldMapping extends FieldMapping {
    def withParent(tpe: Type): FieldMapping = this
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
          if (focus.asString.map(e.hasValue).getOrElse(false)) focus.success
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

    def narrowsTo(subtpe: TypeRef): Boolean =
      subtpe <:< tpe &&
        ((subtpe.dealias, focus.asObject) match {
          case (nt: TypeWithFields, Some(obj)) =>
            nt.fields.forall { f =>
              f.tpe.isNullable || obj.contains(f.name)
            } && obj.keys.forall(nt.hasField)

          case _ => false
        })

    def narrow(subtpe: TypeRef): Result[Cursor] =
      if (narrowsTo(subtpe))
        mkChild(context.asType(subtpe)).success
      else
        Result.internalError(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")

    def hasField(fieldName: String): Boolean =
      fieldMapping(context, fieldName).isDefined ||
      tpe.hasField(fieldName) && focus.asObject.map(_.contains(fieldName)).getOrElse(false)

    def field(fieldName: String, resultName: Option[String]): Result[Cursor] = {
      mkCursorForField(this, fieldName, resultName)
    }
  }
}
