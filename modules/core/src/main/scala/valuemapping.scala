// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import scala.reflect.ClassTag

import cats.Monad
import cats.implicits._
import fs2.Stream
import io.circe.Json
import org.tpolecat.sourcepos.SourcePos

import Cursor.{Context, Env}
import QueryInterpreter.{mkErrorResult, mkOneError}
import ScalarType._

abstract class ValueMapping[F[_]: Monad] extends Mapping[F] {

  case class ValueRoot(otpe: Option[Type], fieldName: String, root: F[Any], mutation: Mutation)(
    implicit val pos: SourcePos
  ) extends RootMapping {
    def cursor(query: Query, env: Env, resultName: Option[String]): Stream[F,Result[(Query, Cursor)]] = {
      (for {
        tpe      <- otpe
        fieldTpe <- tpe.field(fieldName)
      } yield {
        val cursorTpe = query match {
          case _: Query.Unique => fieldTpe.nonNull.list
          case _ => fieldTpe
        }
        Stream.eval(root).map(r => Result((query, ValueCursor(Context(fieldName, resultName, cursorTpe), r, None, env))))
      }).getOrElse(mkErrorResult[(Query, Cursor)](s"Type ${otpe.getOrElse("unspecified type")} has no field '$fieldName'").pure[Stream[F,*]])
    }

    def withParent(tpe: Type): ValueRoot =
      new ValueRoot(Some(tpe), fieldName, root, mutation)
  }

  object ValueRoot {

    // TODO: deprecate
    def apply(fieldName: String, root: Any, mutation: Mutation = Mutation.None)(implicit pos: SourcePos): ValueRoot =
      pure(fieldName, root, mutation)

    /** Construct a `ValueRoot` with constant value `root`. */
    def pure(fieldName: String, root: Any, mutation: Mutation = Mutation.None)(implicit pos: SourcePos): ValueRoot =
      liftF(fieldName, root.pure[F], mutation)

    /** Construct a `ValueRoot` with computed value `root`. */
    def liftF(fieldName: String, root: F[Any], mutation: Mutation = Mutation.None)(implicit pos: SourcePos): ValueRoot =
      new ValueRoot(None, fieldName, root, mutation)

  }

  sealed trait ValueField0[T] extends FieldMapping
  object ValueField0 {
    implicit def wrap[T](fm: FieldMapping): ValueField0[T] = Wrap(fm)
    case class Wrap[T](fm: FieldMapping)(implicit val pos: SourcePos) extends ValueField0[T] {
      def fieldName = fm.fieldName
      def hidden = fm.hidden
      def withParent(tpe: Type): FieldMapping = fm.withParent(tpe)
    }
  }
  case class ValueField[T](fieldName: String, f: T => Any, hidden: Boolean = false)(implicit val pos: SourcePos) extends ValueField0[T] {
    def withParent(tpe: Type): ValueField[T] = this
  }

  case class ValueObjectMapping[T](
    tpe: Type,
    fieldMappings: List[FieldMapping],
    classTag: ClassTag[T]
  )(implicit val pos: SourcePos) extends ObjectMapping

  def ValueObjectMapping[T](
    tpe: Type,
    fieldMappings: List[ValueField0[T]]
  )(implicit classTag: ClassTag[T], pos: SourcePos): ValueObjectMapping[T] =
    new ValueObjectMapping(tpe, fieldMappings.map(_.withParent(tpe)), classTag)

  case class ValueCursor(
    context: Context,
    focus:  Any,
    parent: Option[Cursor],
    env:    Env
  ) extends Cursor {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def mkChild(context: Context = context, focus: Any = focus): ValueCursor =
      ValueCursor(context, focus, Some(this), Env.empty)

    def isLeaf: Boolean =
      tpe.dealias match {
        case (_: ScalarType)|(_: EnumType) => true
        case _ => leafMapping(tpe).isDefined
      }

    def asLeaf: Result[Json] =
      leafMapping(tpe) match {
        case Some(mapping) => mapping.asInstanceOf[LeafMapping[Any]].encoder(focus).rightIor
        case None =>
          (tpe.dealias, focus) match {
            case (StringType,  s: String)  => Json.fromString(s).rightIor
            case (IDType,      s: String)  => Json.fromString(s).rightIor
            case (IntType,     i: Int)     => Json.fromInt(i).rightIor
            case (IntType,     l: Long)    => Json.fromLong(l).rightIor
            case (FloatType,   f: Float)   => Json.fromFloat(f).toRightIor(mkOneError(s"Unrepresentable float %d"))
            case (FloatType,   d: Double)  => Json.fromDouble(d).toRightIor(mkOneError(s"Unrepresentable double %d"))
            case (BooleanType, b: Boolean) => Json.fromBoolean(b).rightIor
            case (_: EnumType, e: Enumeration#Value) => Json.fromString(e.toString).rightIor
            case _ =>
              mkErrorResult(s"Expected Scalar type, found $tpe for focus ${focus}")
          }
      }

    def isList: Boolean = (tpe, focus) match {
      case (_: ListType, _: List[_]) => true
      case _ => false
    }

    def asList: Result[List[Cursor]] = (tpe, focus) match {
      case (ListType(tpe), it: List[_]) => it.map(f => mkChild(context.asType(tpe), f)).rightIor
      case _ => mkErrorResult(s"Expected List type, found $tpe")
    }

    def isNullable: Boolean = (tpe, focus) match {
      case (_: NullableType, _: Option[_]) => true
      case _ => false
    }

    def asNullable: Result[Option[Cursor]] = (tpe, focus) match {
      case (NullableType(tpe), o: Option[_]) => o.map(f => mkChild(context.asType(tpe), f)).rightIor
      case (_: NullableType, _) => mkErrorResult(s"Found non-nullable $focus for $tpe")
      case _ => mkErrorResult(s"Expected Nullable type, found $focus for $tpe")
    }

    def narrowsTo(subtpe: TypeRef): Boolean =
      subtpe <:< tpe &&
        objectMapping(context.asType(subtpe)).map {
          case ValueObjectMapping(_, _, classTag) =>
            classTag.runtimeClass.isInstance(focus)
          case _ => false
        }.getOrElse(false)


    def narrow(subtpe: TypeRef): Result[Cursor] =
      if (narrowsTo(subtpe))
        mkChild(context.asType(subtpe)).rightIor
      else
        mkErrorResult(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")

    def hasField(fieldName: String): Boolean =
      fieldMapping(context, fieldName).isDefined

    def field(fieldName: String, resultName: Option[String]): Result[Cursor] = {
      val fieldContext = context.forFieldOrAttribute(fieldName, resultName)
      fieldMapping(context, fieldName) match {
        case Some(ValueField(_, f, _)) =>
          mkChild(fieldContext, f.asInstanceOf[Any => Any](focus)).rightIor
        case Some(CursorField(_, f, _, _, _)) =>
          f(this).map(res => mkChild(fieldContext, res))
        case _ =>
          mkErrorResult(s"No field '$fieldName' for type $tpe")
      }
    }
  }
}
