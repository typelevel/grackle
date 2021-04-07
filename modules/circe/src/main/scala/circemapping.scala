// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package circe

import cats.Monad
import cats.implicits._
import io.circe.Json

import Cursor.Env
import QueryInterpreter.{mkErrorResult, mkOneError}
import ScalarType._
import org.tpolecat.sourcepos.SourcePos

abstract class CirceMapping[F[_]: Monad] extends Mapping[F] {
  case class CirceRoot(val otpe: Option[Type], val fieldName: String, root: Json, mutation: Mutation)(
    implicit val pos: SourcePos
  ) extends RootMapping {
    def cursor(query: Query, env: Env): F[Result[Cursor]] = {
      (for {
        tpe      <- otpe
        fieldTpe <- tpe.field(fieldName)
      } yield {
        val cursorTpe = query match {
          case _: Query.Unique => fieldTpe.nonNull.list
          case _ => fieldTpe
        }
        CirceCursor(Nil, cursorTpe, root, None, env).rightIor
      }).getOrElse(mkErrorResult(s"Type ${otpe.getOrElse("unspecified type")} has no field '$fieldName'")).pure[F].widen
    }

    def withParent(tpe: Type): CirceRoot =
      new CirceRoot(Some(tpe), fieldName, root, mutation)
  }

  object CirceRoot {
    def apply(fieldName: String, root: Json, mutation: Mutation = Mutation.None): CirceRoot =
      new CirceRoot(None, fieldName, root, mutation)
  }

  case class CirceCursor(
    path:   List[String],
    tpe:    Type,
    focus:  Json,
    parent: Option[Cursor],
    env:    Env
  ) extends Cursor {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def mkChild(path: List[String] = path, tpe: Type = tpe, focus: Json = focus): CirceCursor =
      CirceCursor(path, tpe, focus, Some(this), Env.empty)

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
        case _ =>
          mkErrorResult(s"Expected Scalar type, found $tpe for focus ${focus.noSpaces}")
      }

    def isList: Boolean = tpe.isList && focus.isArray

    def asList: Result[List[Cursor]] = tpe match {
      case ListType(elemTpe) if focus.isArray =>
        focus.asArray.map(_.map(e => mkChild(tpe = elemTpe, focus = e)).toList)
          .toRightIor(mkOneError(s"Expected List type, found $tpe for focus ${focus.noSpaces}"))
      case _ =>
        mkErrorResult(s"Expected List type, found $tpe for focus ${focus.noSpaces}")
    }

    def isNullable: Boolean = tpe.isNullable

    def asNullable: Result[Option[Cursor]] = tpe match {
      case NullableType(tpe) =>
        if (focus.isNull) None.rightIor
        else Some(mkChild(tpe = tpe)).rightIor
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
        mkChild(tpe = subtpe).rightIor
      else
        mkErrorResult(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")

    def hasField(fieldName: String): Boolean =
      tpe.hasField(fieldName) && focus.asObject.map(_.contains(fieldName)).getOrElse(false)

    def field(fieldName: String): Result[Cursor] = {
      val f = focus.asObject.flatMap(_(fieldName))
      (tpe.field(fieldName), f) match {
        case (Some(ftpe), None) if ftpe.isNullable => mkChild(path = fieldName :: path, tpe = ftpe, focus = Json.Null).rightIor
        case (Some(ftpe), Some(json)) => mkChild(path = fieldName :: path, tpe = ftpe, focus = json).rightIor
        case _ =>
          mkErrorResult(s"No field '$fieldName' for type $tpe")
      }
    }

    def hasAttribute(attrName: String): Boolean = false

    def attribute(attrName: String): Result[Any] =
      mkErrorResult(s"No attribute '$attrName' for type $tpe")
  }
}
