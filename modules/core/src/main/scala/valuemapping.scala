// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import scala.collection.Factory
import scala.reflect.ClassTag

import cats.Monad
import cats.implicits._
import io.circe.Json
import org.tpolecat.sourcepos.SourcePos

import Cursor.{Context, Env}
import QueryInterpreter.mkErrorResult

abstract class ValueMapping[F[_]](implicit val M: Monad[F]) extends Mapping[F] with ValueMappingLike[F]

trait ValueMappingLike[F[_]] extends Mapping[F] {
  def valueCursor[T](tpe: Type, env: Env, t: T): Cursor =
    ValueCursor(Context(tpe), t, None, env)

  override def mkCursorForField(parent: Cursor, fieldName: String, resultName: Option[String]): Result[Cursor] = {
    val context = parent.context
    val fieldContext = context.forFieldOrAttribute(fieldName, resultName)
    fieldMapping(context, fieldName) match {
      case Some(ValueField(_, f, _)) =>
        val parentFocus: Any = parent match {
          case vc: ValueCursor => vc.focus
          case _ => ()
        }
        val childFocus = f.asInstanceOf[Any => Any](parentFocus)
        if(isLeaf(fieldContext.tpe))
          LeafCursor(fieldContext, childFocus, Some(parent), parent.env).rightIor
        else
          ValueCursor(fieldContext, childFocus, Some(parent), parent.env).rightIor

      case _ =>
        super.mkCursorForField(parent, fieldName, resultName)
    }
  }

  sealed trait ValueFieldMapping[T] extends FieldMapping
  object ValueFieldMapping {
    implicit def wrap[T](fm: FieldMapping): ValueFieldMapping[T] = Wrap(fm)
    case class Wrap[T](fm: FieldMapping)(implicit val pos: SourcePos) extends ValueFieldMapping[T] {
      def fieldName = fm.fieldName
      def hidden = fm.hidden
      def withParent(tpe: Type): FieldMapping = fm.withParent(tpe)
    }
  }
  case class ValueField[T](fieldName: String, f: T => Any, hidden: Boolean = false)(implicit val pos: SourcePos) extends ValueFieldMapping[T] {
    def withParent(tpe: Type): ValueField[T] = this
  }
  object ValueField {
    def fromValue[T](fieldName: String, t: T, hidden: Boolean = false)(implicit pos: SourcePos): ValueField[Unit] =
      new ValueField[Unit](fieldName, _ => t, hidden)
  }

  case class ValueObjectMapping[T](
    tpe: Type,
    fieldMappings: List[FieldMapping],
    classTag: ClassTag[T]
  )(implicit val pos: SourcePos) extends ObjectMapping

  def ValueObjectMapping[T](
    tpe: Type,
    fieldMappings: List[ValueFieldMapping[T]]
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

    def isLeaf: Boolean = false
    def asLeaf: Result[Json] =
      mkErrorResult(s"Not a leaf: $tpe")

    def preunique: Result[Cursor] = {
      val listTpe = tpe.nonNull.list
      focus match {
        case _: List[_] => mkChild(context.asType(listTpe), focus).rightIor
        case _ =>
          mkErrorResult(s"Expected List type, found $focus for ${listTpe}")
      }
    }

    def isList: Boolean = (tpe, focus) match {
      case (_: ListType, _: List[_]) => true
      case _ => false
    }

    def asList[C](factory: Factory[Cursor, C]): Result[C] = (tpe, focus) match {
      case (ListType(tpe), it: List[_]) => it.view.map(f => mkChild(context.asType(tpe), f)).to(factory).rightIor
      case _ => mkErrorResult(s"Expected List type, found $tpe")
    }

    def listSize: Result[Int] = (tpe, focus) match {
      case (ListType(_), it: List[_]) => it.size.rightIor
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

    def isDefined: Result[Boolean] = (tpe, focus) match {
      case (_: NullableType, opt: Option[_]) => opt.isDefined.rightIor
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

    def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      mkCursorForField(this, fieldName, resultName)
  }
}
