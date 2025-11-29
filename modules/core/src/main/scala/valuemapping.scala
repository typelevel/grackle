// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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

import scala.collection.Factory
import scala.reflect.ClassTag

import cats.MonadThrow
import io.circe.Json
import org.tpolecat.sourcepos.SourcePos

import syntax._
import Cursor.{DeferredCursor}

abstract class ValueMapping[F[_]](implicit val M: MonadThrow[F]) extends Mapping[F] with ValueMappingLike[F]

trait ValueMappingLike[F[_]] extends Mapping[F] {
  import typeMappings._

  def mkCursor(context: Context, focus: Any, parent: Option[Cursor], env: Env): Cursor =
    if(context.tpe.isUnderlyingLeaf)
      LeafCursor(context, focus, parent, env)
    else
      ValueCursor(context, focus, parent, env)

  def valueCursor[T](path: Path, env: Env, t: T): Cursor =
    if(path.path.isEmpty)
      mkCursor(Context(path.rootTpe), t, None, env)
    else {
      DeferredCursor(path, (context, parent) => mkCursor(context, t, Some(parent), env).success)
    }

  override def mkCursorForMappedField(parent: Cursor, fieldContext: Context, fm: FieldMapping): Result[Cursor] =
    fm match {
      case ValueField(_, f, _) =>
        val parentFocus: Any = parent match {
          case vc: ValueCursor => vc.focus
          case _ => ()
        }
        val childFocus = f.asInstanceOf[Any => Any](parentFocus)
        mkCursor(fieldContext, childFocus, Some(parent), Env.empty).success

      case _ =>
        super.mkCursorForMappedField(parent, fieldContext, fm)
    }

  sealed trait ValueFieldMapping[T] extends FieldMapping {
    def unwrap: FieldMapping
  }

  object ValueFieldMapping {
    implicit def wrap[T](fm: FieldMapping): ValueFieldMapping[T] = Wrap(fm)
    case class Wrap[T](fm: FieldMapping)(implicit val pos: SourcePos) extends ValueFieldMapping[T] {
      def fieldName = fm.fieldName
      def hidden = fm.hidden
      def subtree = fm.subtree
      def unwrap = fm
    }
  }
  case class ValueField[T](fieldName: String, f: T => Any, hidden: Boolean = false)(implicit val pos: SourcePos) extends ValueFieldMapping[T] {
    def subtree: Boolean = false
    def unwrap: FieldMapping = this
  }
  object ValueField {
    def fromValue[T](fieldName: String, t: T, hidden: Boolean = false)(implicit pos: SourcePos): ValueField[Unit] =
      new ValueField[Unit](fieldName, _ => t, hidden)
  }

  object ValueObjectMapping {
    case class DefaultValueObjectMapping(
      predicate: MappingPredicate,
      fieldMappings: Seq[FieldMapping],
      classTag: ClassTag[_]
    )(implicit val pos: SourcePos) extends ObjectMapping {
      override def showMappingType: String = "ValueObjectMapping"
    }

    class Builder(predicate: MappingPredicate, pos: SourcePos) {
      def on[T](fieldMappings: ValueFieldMapping[T]*)(implicit classTag: ClassTag[T]): ObjectMapping =
        DefaultValueObjectMapping(predicate, fieldMappings.map(_.unwrap), classTag)(pos)
    }

    def apply(predicate: MappingPredicate)(implicit pos: SourcePos): Builder =
      new Builder(predicate, pos)

    def apply(tpe: NamedType)(implicit pos: SourcePos): Builder =
      new Builder(MappingPredicate.TypeMatch(tpe), pos)

    def apply(path: Path)(implicit pos: SourcePos): Builder =
      new Builder(MappingPredicate.PathMatch(path), pos)

    def apply[T](
      tpe: NamedType,
      fieldMappings: List[ValueFieldMapping[T]]
    )(implicit classTag: ClassTag[T], pos: SourcePos): ObjectMapping =
      DefaultValueObjectMapping(MappingPredicate.TypeMatch(tpe), fieldMappings.map(_.unwrap), classTag)

    def unapply(om: DefaultValueObjectMapping): Option[(MappingPredicate, Seq[FieldMapping], ClassTag[_])] = {
      Some((om.predicate, om.fieldMappings, om.classTag))
    }
  }

  override protected def unpackPrefixedMapping(prefix: List[String], om: ObjectMapping): ObjectMapping =
    om match {
      case vom: ValueObjectMapping.DefaultValueObjectMapping =>
        vom.copy(predicate = MappingPredicate.PrefixedTypeMatch(prefix, om.predicate.tpe))
      case _ => super.unpackPrefixedMapping(prefix, om)
    }

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
      Result.internalError(s"Not a leaf: $tpe")

    def preunique: Result[Cursor] = {
      val listTpe = tpe.nonNull.list
      focus match {
        case _: List[_] => mkChild(context.asType(listTpe), focus).success
        case _ =>
          Result.internalError(s"Expected List type, found $focus for ${listTpe}")
      }
    }

    def isList: Boolean = (tpe, focus) match {
      case (_: ListType, _: List[_]) => true
      case _ => false
    }

    def asList[C](factory: Factory[Cursor, C]): Result[C] = (tpe, focus) match {
      case (ListType(tpe), it: List[_]) => it.view.map(f => mkChild(context.asType(tpe), f)).to(factory).success
      case _ => Result.internalError(s"Expected List type, found $tpe")
    }

    def listSize: Result[Int] = (tpe, focus) match {
      case (ListType(_), it: List[_]) => it.size.success
      case _ => Result.internalError(s"Expected List type, found $tpe")
    }

    def isNullable: Boolean = (tpe, focus) match {
      case (_: NullableType, _: Option[_]) => true
      case _ => false
    }

    def asNullable: Result[Option[Cursor]] = (tpe, focus) match {
      case (NullableType(tpe), o: Option[_]) => o.map(f => mkChild(context.asType(tpe), f)).success
      case (_: NullableType, _) => Result.internalError(s"Found non-nullable $focus for $tpe")
      case _ => Result.internalError(s"Expected Nullable type, found $focus for $tpe")
    }

    def isDefined: Result[Boolean] = (tpe, focus) match {
      case (_: NullableType, opt: Option[_]) => opt.isDefined.success
      case _ => Result.internalError(s"Expected Nullable type, found $focus for $tpe")
    }

    def narrowsTo(subtpe: TypeRef): Result[Boolean] =
      (subtpe <:< tpe &&
        objectMapping(context.asType(subtpe)).exists {
          case ValueObjectMapping(_, _, classTag) =>
            classTag.runtimeClass.isInstance(focus)
          case _ => false
        }).success


    def narrow(subtpe: TypeRef): Result[Cursor] =
      narrowsTo(subtpe).flatMap { n =>
        if(n)
          mkChild(context.asType(subtpe)).success
        else
          Result.internalError(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")
      }

    def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      mkCursorForField(this, fieldName, resultName)
  }
}
