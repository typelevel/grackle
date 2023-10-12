// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle
package generic

import scala.collection.Factory

import cats.implicits._
import io.circe.{Encoder, Json}

import syntax._
import Cursor.AbstractCursor

trait CursorBuilder[T] {
  def tpe: Type
  def build(context: Context, focus: T, parent: Option[Cursor] = None, env: Env = Env.empty): Result[Cursor]

  /**
   *  Apply a pre-processing function while fixing the `Type` of this `CursorBuilder`.
   */
  final def contramap[A](f: A => T): CursorBuilder[A] = CursorBuilder.contramap(f, this)
}

object CursorBuilder {
  def apply[T](implicit cb: CursorBuilder[T]): CursorBuilder[T] = cb

  import ScalarType._

  implicit val stringCursorBuilder: CursorBuilder[String] = {
    case class StringCursor(context: Context, focus: String, parent: Option[Cursor], env: Env)
        extends PrimitiveCursor[String] {
      def withEnv(env0: Env): Cursor    = copy(env = env.add(env0))
      override def asLeaf: Result[Json] = Json.fromString(focus).success
    }
    new CursorBuilder[String] {
      val tpe = StringType
      def build(context: Context, focus: String, parent: Option[Cursor], env: Env): Result[Cursor] =
        StringCursor(context.asType(tpe), focus, parent, env).success
    }
  }

  implicit val intCursorBuilder: CursorBuilder[Int] = {
    case class IntCursor(context: Context, focus: Int, parent: Option[Cursor], env: Env) extends PrimitiveCursor[Int] {
      def withEnv(env0: Env): Cursor    = copy(env = env.add(env0))
      override def asLeaf: Result[Json] = Json.fromInt(focus).success
    }
    new CursorBuilder[Int] {
      val tpe = IntType
      def build(context: Context, focus: Int, parent: Option[Cursor], env: Env): Result[Cursor] =
        IntCursor(context.asType(tpe), focus, parent, env).success
    }
  }

  implicit val longCursorBuilder: CursorBuilder[Long] = {
    case class LongCursor(context: Context, focus: Long, parent: Option[Cursor], env: Env)
        extends PrimitiveCursor[Long] {
      def withEnv(env0: Env): Cursor    = copy(env = env.add(env0))
      override def asLeaf: Result[Json] = Json.fromLong(focus).success
    }
    new CursorBuilder[Long] {
      val tpe = IntType
      def build(context: Context, focus: Long, parent: Option[Cursor], env: Env): Result[Cursor] =
        LongCursor(context.asType(tpe), focus, parent, env).success
    }
  }

  implicit val floatCursorBuilder: CursorBuilder[Float] = {
    case class FloatCursor(context: Context, focus: Float, parent: Option[Cursor], env: Env)
        extends PrimitiveCursor[Float] {
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
      override def asLeaf: Result[Json] =
        Json.fromFloat(focus).toResultOrError(s"Unrepresentable float %focus")
    }
    new CursorBuilder[Float] {
      val tpe = FloatType
      def build(context: Context, focus: Float, parent: Option[Cursor], env: Env): Result[Cursor] =
        FloatCursor(context.asType(tpe), focus, parent, env).success
    }
  }

  implicit val doubleCursorBuilder: CursorBuilder[Double] = {
    case class DoubleCursor(context: Context, focus: Double, parent: Option[Cursor], env: Env)
        extends PrimitiveCursor[Double] {
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
      override def asLeaf: Result[Json] =
        Json.fromDouble(focus).toResultOrError(s"Unrepresentable double %focus")
    }
    new CursorBuilder[Double] {
      val tpe = FloatType
      def build(context: Context, focus: Double, parent: Option[Cursor], env: Env): Result[Cursor] =
        DoubleCursor(context.asType(tpe), focus, parent, env).success
    }
  }

  implicit val booleanCursorBuilder: CursorBuilder[Boolean] = {
    case class BooleanCursor(context: Context, focus: Boolean, parent: Option[Cursor], env: Env)
        extends PrimitiveCursor[Boolean] {
      def withEnv(env0: Env): Cursor    = copy(env = env.add(env0))
      override def asLeaf: Result[Json] = Json.fromBoolean(focus).success
    }
    new CursorBuilder[Boolean] {
      val tpe = BooleanType
      def build(context: Context, focus: Boolean, parent: Option[Cursor], env: Env): Result[Cursor] =
        BooleanCursor(context.asType(tpe), focus, parent, env).success
    }
  }

  def deriveEnumerationCursorBuilder[T <: Enumeration#Value](tpe0: Type): CursorBuilder[T] = {
    case class EnumerationCursor(context: Context, focus: T, parent: Option[Cursor], env: Env)
        extends PrimitiveCursor[T] {
      def withEnv(env0: Env): Cursor    = copy(env = env.add(env0))
      override def asLeaf: Result[Json] = Json.fromString(focus.toString).success
    }
    new CursorBuilder[T] {
      val tpe = tpe0
      def build(context: Context, focus: T, parent: Option[Cursor], env: Env): Result[Cursor] =
        EnumerationCursor(context.asType(tpe), focus, parent, env).success
    }
  }

  implicit def enumerationCursorBuilder[T <: Enumeration#Value]: CursorBuilder[T] =
    deriveEnumerationCursorBuilder(StringType)

  implicit def optionCursorBuiler[T](implicit elemBuilder: CursorBuilder[T]): CursorBuilder[Option[T]] = {
    case class OptionCursor(context: Context, focus: Option[T], parent: Option[Cursor], env: Env)
        extends AbstractCursor {
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

      override def isNullable: Boolean = true
      override def asNullable: Result[Option[Cursor]] =
        focus.traverse(elem => elemBuilder.build(context, elem, Some(this), env))
    }

    new CursorBuilder[Option[T]] { outer =>
      val tpe = NullableType(elemBuilder.tpe)
      def build(context: Context, focus: Option[T], parent: Option[Cursor], env: Env): Result[Cursor] =
        OptionCursor(context.asType(tpe), focus, parent, env).success
    }
  }

  implicit def listCursorBuiler[T](implicit elemBuilder: CursorBuilder[T]): CursorBuilder[List[T]] = {
    case class ListCursor(context: Context, focus: List[T], parent: Option[Cursor], env: Env) extends AbstractCursor {
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

      override def preunique: Result[Cursor] = {
        val listTpe = tpe.nonNull.list
        copy(context = context.asType(listTpe)).success
      }

      override def isList: Boolean = true
      override def asList[C](factory: Factory[Cursor, C]): Result[C] =
        focus.traverse(elem => elemBuilder.build(context, elem, Some(this), env)).map(_.to(factory))
    }

    new CursorBuilder[List[T]] { outer =>
      val tpe = ListType(elemBuilder.tpe)
      def build(context: Context, focus: List[T], parent: Option[Cursor], env: Env): Result[Cursor] =
        ListCursor(context.asType(tpe), focus, parent, env).success
    }
  }

  case class LeafCursor[T](context: Context, focus: T, encoder: Encoder[T], parent: Option[Cursor], env: Env)
      extends AbstractCursor {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    override def isLeaf: Boolean      = true
    override def asLeaf: Result[Json] = encoder(focus).success
  }

  def deriveLeafCursorBuilder[T](tpe0: Type)(implicit encoder: Encoder[T]): CursorBuilder[T] =
    new CursorBuilder[T] {
      val tpe = tpe0
      def build(context: Context, focus: T, parent: Option[Cursor], env: Env): Result[Cursor] =
        new LeafCursor(context.asType(tpe), focus, encoder, parent, env).success
    }

  implicit def leafCursorBuilder[T](implicit encoder: Encoder[T]): CursorBuilder[T] =
    deriveLeafCursorBuilder(StringType)

  def contramap[A, B](f: A => B, cb: CursorBuilder[B]): CursorBuilder[A] =
    new CursorBuilder[A] {
      def tpe: Type = cb.tpe
      def build(context: Context, focus: A, parent: Option[Cursor] = None, env: Env = Env.empty): Result[Cursor] =
        cb.build(context, f(focus), parent, env)
    }
}

abstract class PrimitiveCursor[T] extends AbstractCursor {
  val focus: T
  override def isLeaf: Boolean = true
}
