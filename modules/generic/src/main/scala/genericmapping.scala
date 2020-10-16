// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import scala.annotation.{ nowarn, tailrec }

import cats.Monad
import cats.implicits._
import io.circe.{ Encoder, Json }
import shapeless.{ Coproduct, Generic, ::, HList, HNil, Inl, Inr, LabelledGeneric }
import shapeless.ops.hlist.{ LiftAll => PLiftAll }
import shapeless.ops.coproduct.{ LiftAll => CLiftAll }
import shapeless.ops.record.Keys

import QueryInterpreter.{ mkErrorResult, mkOneError }
import ShapelessUtils._

trait GenericMapping[F[_]] extends AbstractMapping[Monad, F] {
  case class GenericRoot[T](val tpe: Type, val fieldName: String, t: T, cb: () => CursorBuilder[T]) extends RootMapping {
    lazy val cursorBuilder = cb()
    def cursor(query: Query): F[Result[Cursor]] = cursorBuilder.build(Nil, t).pure[F]
    def withParent(tpe: Type): GenericRoot[T] =
      new GenericRoot(tpe, fieldName, t, cb)
  }

  def GenericRoot[T](fieldName: String, t: T)(implicit cb: => CursorBuilder[T]): GenericRoot[T] =
    new GenericRoot(NoType, fieldName, t, () => cb)
}

object semiauto {
  final def deriveObjectCursorBuilder[T](tpe: Type)
    (implicit mkBuilder: => MkObjectCursorBuilder[T]): ObjectCursorBuilder[T] = mkBuilder(tpe)
  final def deriveInterfaceCursorBuilder[T](tpe: Type)
    (implicit mkBuilder: => MkInterfaceCursorBuilder[T]): CursorBuilder[T] = mkBuilder(tpe)
}

trait ObjectCursorBuilder[T] extends CursorBuilder[T] {
  def renameField(from: String, to: String): ObjectCursorBuilder[T]
  def transformFieldNames(f: String => String): ObjectCursorBuilder[T]
  def transformField[U](fieldName: String)(f: T => Result[U])(implicit cb: => CursorBuilder[U]): ObjectCursorBuilder[T]
}

trait MkObjectCursorBuilder[T] {
  def apply(tpe: Type): ObjectCursorBuilder[T]
}

object MkObjectCursorBuilder {
  implicit def productCursorBuilder[T <: Product, R <: HList, L <: HList]
    (implicit
      @nowarn gen: Generic.Aux[T, R],
      elems0: => PLiftAll[CursorBuilder, R],
      @nowarn lgen: LabelledGeneric.Aux[T, L],
      keys0: Keys[L]
    ): MkObjectCursorBuilder[T] =
    new MkObjectCursorBuilder[T] {
      def apply(tpe: Type): ObjectCursorBuilder[T] = {
        def fieldMap: Map[String, (List[String], T) => Result[Cursor]] = {
          val keys: List[String] = unsafeToList[Symbol](keys0()).map(_.name)
          val elems = unsafeToList[CursorBuilder[Any]](elems0.instances)
          keys.zip(elems.zipWithIndex).map {
            case (fieldName, (elem, idx)) => (fieldName, (p: List[String], t: T) => elem.build(p, t.productElement(idx)))
          }.toMap
        }
        new Impl[T](tpe, fieldMap)
      }
    }

  class Impl[T](tpe0: Type, fieldMap0: => Map[String, (List[String], T) => Result[Cursor]]) extends ObjectCursorBuilder[T] {
    lazy val fieldMap = fieldMap0

    val tpe = tpe0

    def build(path: List[String], focus: T): Result[Cursor] =
      CursorImpl(tpe, fieldMap, path, focus).rightIor

    def renameField(from: String, to: String): ObjectCursorBuilder[T] =
      transformFieldNames { case `from` => to ; case other => other }
    def transformFieldNames(f: String => String): ObjectCursorBuilder[T] =
      new Impl(tpe, fieldMap0.map { case (k, v) => (f(k), v) })
    def transformField[U](fieldName: String)(f: T => Result[U])(implicit cb: => CursorBuilder[U]): ObjectCursorBuilder[T] =
      new Impl(tpe, fieldMap0.updated(fieldName, (path: List[String], focus: T) => f(focus).flatMap(f => cb.build(path, f))))
  }

  case class CursorImpl[T](tpe: Type, fieldMap: Map[String, (List[String], T) => Result[Cursor]], path: List[String], focus: T)
    extends AbstractCursor[Product] {
    override def hasField(fieldName: String): Boolean = fieldMap.contains(fieldName)

    override def field(fieldName: String): Result[Cursor] = {
      fieldMap.get(fieldName).toRightIor(mkOneError(s"No field '$fieldName' for type $tpe")).flatMap(f => f(fieldName :: path, focus))
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
    def build(path: List[String], focus: T): Result[Cursor] = {
      val builder = elems(sel(focus))
      builder.build(path, focus).map(cursor => CursorImpl(tpe, builder.tpe, cursor))
    }
  }

  case class CursorImpl[T](tpe: Type, rtpe: Type, cursor: Cursor)
    extends AbstractCursor[T] {
    def focus: Any = cursor.focus
    def path: List[String] = cursor.path

    override def hasField(fieldName: String): Boolean = cursor.hasField(fieldName)

    override def field(fieldName: String): Result[Cursor] = cursor.field(fieldName)

    override def narrowsTo(subtpe: TypeRef): Boolean =
      subtpe <:< tpe && rtpe <:< subtpe

    override def narrow(subtpe: TypeRef): Result[Cursor] =
      if (narrowsTo(subtpe)) copy(tpe = subtpe).rightIor
      else mkErrorResult(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")
  }
}

trait CursorBuilder[T] { outer =>
  def tpe: Type
  def build(path: List[String], focus: T): Result[Cursor]
}

object CursorBuilder {
  def apply[T](implicit cb: CursorBuilder[T]): CursorBuilder[T] = cb

  import ScalarType._

  implicit val stringCursorBuilder: CursorBuilder[String] =
    new CursorBuilder[String] {
      val tpe = StringType
      def build(path: List[String], focus: String): Result[Cursor] =
        new PrimitiveCursor(focus, tpe, path) {
          override def asLeaf: Result[Json] = Json.fromString(focus).rightIor
        }.rightIor
    }

  implicit val intCursorBuilder: CursorBuilder[Int] =
    new CursorBuilder[Int] {
      val tpe = IntType
      def build(path: List[String], focus: Int): Result[Cursor] =
        new PrimitiveCursor(focus, tpe, path) {
          override def asLeaf: Result[Json] = Json.fromInt(focus).rightIor
        }.rightIor
    }

  implicit val longCursorBuilder: CursorBuilder[Long] =
    new CursorBuilder[Long] {
      val tpe = IntType
      def build(path: List[String], focus: Long): Result[Cursor] =
        new PrimitiveCursor(focus, tpe, path) {
          override def asLeaf: Result[Json] = Json.fromLong(focus).rightIor
        }.rightIor
    }

  implicit val floatCursorBuilder: CursorBuilder[Float] =
    new CursorBuilder[Float] {
      val tpe = FloatType
      def build(path: List[String], focus: Float): Result[Cursor] =
        new PrimitiveCursor(focus, tpe, path) {
          override def asLeaf: Result[Json] =
            Json.fromFloat(focus).toRightIor(mkOneError(s"Unrepresentable float %focus"))
        }.rightIor
    }

  implicit val doubleCursorBuilder: CursorBuilder[Double] =
    new CursorBuilder[Double] {
      val tpe = FloatType
      def build(path: List[String], focus: Double): Result[Cursor] =
        new PrimitiveCursor(focus, tpe, path) {
          override def asLeaf: Result[Json] =
            Json.fromDouble(focus).toRightIor(mkOneError(s"Unrepresentable double %focus"))
        }.rightIor
    }

  implicit val booleanCursorBuilder: CursorBuilder[Boolean] =
    new CursorBuilder[Boolean] {
      val tpe = BooleanType
      def build(path: List[String], focus: Boolean): Result[Cursor] =
        new PrimitiveCursor(focus, tpe, path) {
          override def asLeaf: Result[Json] = Json.fromBoolean(focus).rightIor
        }.rightIor
    }

  implicit def deriveEnumerationCursorBuilder[T <: Enumeration#Value](tpe0: Type): CursorBuilder[T] =
    new CursorBuilder[T] {
      val tpe = tpe0
      def build(path: List[String], focus: T): Result[Cursor] =
        new PrimitiveCursor(focus, tpe, path) {
          override def asLeaf: Result[Json] = Json.fromString(focus.toString).rightIor
        }.rightIor
    }

  implicit def enumerationCursorBuilder[T <: Enumeration#Value]: CursorBuilder[T] =
    deriveEnumerationCursorBuilder(NoType)

  implicit def optionCursorBuiler[T](implicit elemBuilder: CursorBuilder[T]): CursorBuilder[Option[T]] =
    new CursorBuilder[Option[T]] { outer =>
      val tpe = NullableType(elemBuilder.tpe)
      def build(path0: List[String], focus0: Option[T]): Result[Cursor] =
        new AbstractCursor[Option[T]] {
          def focus = focus0
          def tpe = outer.tpe
          def path = path0

          override def isNullable: Boolean = true
          override def asNullable: Result[Option[Cursor]] = {
            focus.traverse(elem => elemBuilder.build(path, elem))
          }
        }.rightIor
    }

  implicit def listCursorBuiler[T](implicit elemBuilder: CursorBuilder[T]): CursorBuilder[List[T]] =
    new CursorBuilder[List[T]] { outer =>
      val tpe = ListType(elemBuilder.tpe)
      def build(path0: List[String], focus0: List[T]): Result[Cursor] =
        new AbstractCursor[List[T]] {
          def focus = focus0
          def tpe = outer.tpe
          def path = path0

          override def isList: Boolean = true
          override def asList: Result[List[Cursor]] = {
            focus.traverse(elem => elemBuilder.build(path, elem))
          }
        }.rightIor
    }

  class LeafCursor[T](val focus: T, val tpe: Type, val path: List[String], encoder: Encoder[T]) extends AbstractCursor[T] {
    override def isLeaf: Boolean = true
    override def asLeaf: Result[Json] = encoder(focus).rightIor
  }

  def deriveLeafCursorBuilder[T](tpe0: Type)(implicit encoder: Encoder[T]): CursorBuilder[T] =
    new CursorBuilder[T] {
      val tpe = tpe0
      def build(path: List[String], focus: T): Result[Cursor] =
        new LeafCursor(focus, tpe, path, encoder).rightIor
    }

  implicit def leafCursorBuilder[T](implicit encoder: Encoder[T]): CursorBuilder[T] =
    deriveLeafCursorBuilder(NoType)
}

abstract class AbstractCursor[T] extends Cursor {
  def isLeaf: Boolean = false

  def asLeaf: Result[Json] =
    mkErrorResult(s"Expected Scalar type, found $tpe for focus ${focus}")

  def isList: Boolean = false

  def asList: Result[List[Cursor]] =
    mkErrorResult(s"Expected List type, found $tpe")

  def isNullable: Boolean = false

  def asNullable: Result[Option[Cursor]] =
    mkErrorResult(s"Expected Nullable type, found $focus for $tpe")

  def narrowsTo(subtpe: TypeRef): Boolean = false

  def narrow(subtpe: TypeRef): Result[Cursor] =
    mkErrorResult(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")

  def hasField(fieldName: String): Boolean = false

  def field(fieldName: String): Result[Cursor] =
    mkErrorResult(s"No field '$fieldName' for type $tpe")

  def hasAttribute(attributeName: String): Boolean = false

  def attribute(attributeName: String): Result[Any] =
    mkErrorResult(s"No attribute '$attributeName' for type $tpe")
}

abstract class PrimitiveCursor[T](val focus: T, val tpe: Type, val path: List[String]) extends AbstractCursor[T] {
  override def isLeaf: Boolean = true
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

