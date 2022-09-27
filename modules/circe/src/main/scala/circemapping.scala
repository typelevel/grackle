// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package circe

import scala.collection.Factory

import cats.Monad
import cats.implicits._
import fs2.Stream
import io.circe.Json
import org.tpolecat.sourcepos.SourcePos

import Cursor.{Context, Env}
import QueryInterpreter.{mkErrorResult, mkOneError}
import ScalarType._

abstract class CirceMapping[F[_]: Monad] extends Mapping[F] {
  case class CirceRoot(val otpe: Option[Type], val fieldName: String, root: Json, mutation: Mutation)(
    implicit val pos: SourcePos
  ) extends RootMapping {
    def cursor(query: Query, env: Env, resultName: Option[String]): Stream[F,Result[(Query, Cursor)]] = {
      (for {
        tpe      <- otpe
        context0 <- Context(tpe, fieldName, resultName)
      } yield {
        val context = query match {
          case _: Query.Unique => context0.asType(context0.tpe.nonNull.list)
          case _ => context0
        }
        (query, CirceCursor(context, root, None, env)).rightIor
      }).getOrElse(mkErrorResult(s"Type ${otpe.getOrElse("unspecified type")} has no field '$fieldName'")).pure[Stream[F,*]]
    }

    def withParent(tpe: Type): CirceRoot =
      new CirceRoot(Some(tpe), fieldName, root, mutation)
  }

  object CirceRoot {
    def apply(fieldName: String, root: Json, mutation: Mutation = Mutation.None): CirceRoot =
      new CirceRoot(None, fieldName, root, mutation)
  }

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
        case BooleanType       if focus.isBoolean => focus.rightIor
        case StringType|IDType if focus.isString  => focus.rightIor
        case IntType           if focus.isNumber  =>
          focus.asNumber.flatMap(_.toLong.map(Json.fromLong))
            .toRightIor(mkOneError(s"Expected Int found ${focus.noSpaces}"))
        case FloatType         if focus.isNumber  => focus.rightIor
        case e: EnumType       if focus.isString  =>
          if (focus.asString.map(e.hasValue).getOrElse(false)) focus.rightIor
          else mkErrorResult(s"Expected Enum ${e.name}, found ${focus.noSpaces}")
        case _: ScalarType     if !focus.isObject => focus.rightIor // custom Scalar; any non-object type is fine
        case _ =>
          mkErrorResult(s"Expected Scalar type, found $tpe for focus ${focus.noSpaces} at ${context.path.reverse.mkString("/")} ")
      }

    def preunique: Result[Cursor] = {
      val listTpe = tpe.nonNull.list
      if(focus.isArray)
        mkChild(context.asType(listTpe), focus).rightIor
      else
        mkErrorResult(s"Expected List type, found $focus for ${listTpe}")
    }

    def isList: Boolean = tpe.isList && focus.isArray

    def asList[C](factory: Factory[Cursor, C]): Result[C] = tpe match {
      case ListType(elemTpe) if focus.isArray =>
        focus.asArray.map(_.view.map(e => mkChild(context.asType(elemTpe), e)).to(factory))
          .toRightIor(mkOneError(s"Expected List type, found $tpe for focus ${focus.noSpaces}"))
      case _ =>
        mkErrorResult(s"Expected List type, found $tpe for focus ${focus.noSpaces}")
    }

    def isNullable: Boolean = tpe.isNullable

    def asNullable: Result[Option[Cursor]] = tpe match {
      case NullableType(tpe) =>
        if (focus.isNull) None.rightIor
        else Some(mkChild(context.asType(tpe))).rightIor
      case _ => mkErrorResult(s"Expected Nullable type, found $focus for $tpe")
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
        mkChild(context.asType(subtpe)).rightIor
      else
        mkErrorResult(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")

    def hasField(fieldName: String): Boolean =
      tpe.hasField(fieldName) && focus.asObject.map(_.contains(fieldName)).getOrElse(false)

    def field(fieldName: String, resultName: Option[String]): Result[Cursor] = {
      val f = focus.asObject.flatMap(_(fieldName))
      (context.forField(fieldName, resultName), f) match {
        case (Some(fieldContext), None) if fieldContext.tpe.isNullable => mkChild(fieldContext, Json.Null).rightIor
        case (Some(fieldContext), Some(json)) => mkChild(fieldContext, json).rightIor
        case _ =>
          mkErrorResult(s"No field '$fieldName' for type $tpe")
      }
    }
  }
}
