// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import scala.collection.Factory

import cats.Monad
import cats.implicits._
import io.circe.{Encoder, Json}
import org.tpolecat.sourcepos.SourcePos

import Cursor.{AbstractCursor, Context, DeferredCursor, Env}
import QueryInterpreter.mkOneError

abstract class GenericMapping[F[_]](implicit val M: Monad[F]) extends Mapping[F] with GenericMappingLike[F]

trait GenericMappingLike[F[_]] extends ScalaVersionSpecificGenericMappingLike[F] {
  def genericCursor[T](path: Path, env: Env, t: T)(implicit cb: => CursorBuilder[T]): Result[Cursor] =
    if(path.isRoot)
      cb.build(Context(path.rootTpe), t, None, env)
    else
      DeferredCursor(path, (context, parent) => cb.build(context, t, Some(parent), env)).rightIor

  override def mkCursorForField(parent: Cursor, fieldName: String, resultName: Option[String]): Result[Cursor] = {
    val context = parent.context
    val fieldContext = context.forFieldOrAttribute(fieldName, resultName)
    fieldMapping(context, fieldName) match {
      case Some(GenericField(_, _, t, cb, _)) =>
        cb().build(fieldContext, t, Some(parent), parent.env)
      case _ =>
        super.mkCursorForField(parent, fieldName, resultName)
    }
  }

  case class GenericField[T](val tpe: Option[Type], val fieldName: String, t: T, cb: () => CursorBuilder[T], hidden: Boolean)(
    implicit val pos: SourcePos
  ) extends FieldMapping {
    def withParent(tpe: Type): GenericField[T] =
      new GenericField(Some(tpe), fieldName, t, cb, hidden)
  }

  def GenericField[T](fieldName: String, t: T, hidden: Boolean = true)(implicit cb: => CursorBuilder[T], pos: SourcePos): GenericField[T] =
    new GenericField(None, fieldName, t, () => cb, hidden)

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

  trait CursorBuilder[T] { outer =>
    def tpe: Type
    def build(context: Context, focus: T, parent: Option[Cursor] = None, env: Env = Env.empty): Result[Cursor]
  }

  object CursorBuilder {
    def apply[T](implicit cb: CursorBuilder[T]): CursorBuilder[T] = cb

    import ScalarType._

    implicit val stringCursorBuilder: CursorBuilder[String] = {
      case class StringCursor(context: Context, focus: String, parent: Option[Cursor], env: Env) extends PrimitiveCursor[String] {
        def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
        override def asLeaf: Result[Json] = Json.fromString(focus).rightIor
      }
      new CursorBuilder[String] {
        val tpe = StringType
        def build(context: Context, focus: String, parent: Option[Cursor], env: Env): Result[Cursor] =
          StringCursor(context.asType(tpe), focus, parent, env).rightIor
      }
    }

    implicit val intCursorBuilder: CursorBuilder[Int] = {
      case class IntCursor(context: Context, focus: Int, parent: Option[Cursor], env: Env) extends PrimitiveCursor[Int] {
        def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
        override def asLeaf: Result[Json] = Json.fromInt(focus).rightIor
      }
      new CursorBuilder[Int] {
        val tpe = IntType
        def build(context: Context, focus: Int, parent: Option[Cursor], env: Env): Result[Cursor] =
          IntCursor(context.asType(tpe), focus, parent, env).rightIor
      }
    }

    implicit val longCursorBuilder: CursorBuilder[Long] = {
      case class LongCursor(context: Context, focus: Long, parent: Option[Cursor], env: Env) extends PrimitiveCursor[Long] {
        def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
        override def asLeaf: Result[Json] = Json.fromLong(focus).rightIor
      }
      new CursorBuilder[Long] {
        val tpe = IntType
        def build(context: Context, focus: Long, parent: Option[Cursor], env: Env): Result[Cursor] =
          LongCursor(context.asType(tpe), focus, parent, env).rightIor
      }
    }

    implicit val floatCursorBuilder: CursorBuilder[Float] = {
      case class FloatCursor(context: Context, focus: Float, parent: Option[Cursor], env: Env) extends PrimitiveCursor[Float] {
        def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
        override def asLeaf: Result[Json] =
          Json.fromFloat(focus).toRightIor(mkOneError(s"Unrepresentable float %focus"))
      }
      new CursorBuilder[Float] {
        val tpe = FloatType
        def build(context: Context, focus: Float, parent: Option[Cursor], env: Env): Result[Cursor] =
          FloatCursor(context.asType(tpe), focus, parent, env).rightIor
      }
    }

    implicit val doubleCursorBuilder: CursorBuilder[Double] = {
      case class DoubleCursor(context: Context, focus: Double, parent: Option[Cursor], env: Env) extends PrimitiveCursor[Double] {
        def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
        override def asLeaf: Result[Json] =
          Json.fromDouble(focus).toRightIor(mkOneError(s"Unrepresentable double %focus"))
      }
      new CursorBuilder[Double] {
        val tpe = FloatType
        def build(context: Context, focus: Double, parent: Option[Cursor], env: Env): Result[Cursor] =
          DoubleCursor(context.asType(tpe), focus, parent, env).rightIor
      }
    }

    implicit val booleanCursorBuilder: CursorBuilder[Boolean] = {
      case class BooleanCursor(context: Context, focus: Boolean, parent: Option[Cursor], env: Env) extends PrimitiveCursor[Boolean] {
        def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
        override def asLeaf: Result[Json] = Json.fromBoolean(focus).rightIor
      }
      new CursorBuilder[Boolean] {
        val tpe = BooleanType
        def build(context: Context, focus: Boolean, parent: Option[Cursor], env: Env): Result[Cursor] =
          BooleanCursor(context.asType(tpe), focus, parent, env).rightIor
      }
    }

    def deriveEnumerationCursorBuilder[T <: Enumeration#Value](tpe0: Type): CursorBuilder[T] = {
      case class EnumerationCursor(context: Context, focus: T, parent: Option[Cursor], env: Env) extends PrimitiveCursor[T] {
        def withEnv(env0: Env): Cursor = copy(env = env.add(env0))
        override def asLeaf: Result[Json] = Json.fromString(focus.toString).rightIor
      }
      new CursorBuilder[T] {
        val tpe = tpe0
        def build(context: Context, focus: T, parent: Option[Cursor], env: Env): Result[Cursor] =
          EnumerationCursor(context.asType(tpe), focus, parent, env).rightIor
      }
    }

    implicit def enumerationCursorBuilder[T <: Enumeration#Value]: CursorBuilder[T] =
      deriveEnumerationCursorBuilder(StringType)

    implicit def optionCursorBuiler[T](implicit elemBuilder: CursorBuilder[T]): CursorBuilder[Option[T]] = {
      case class OptionCursor(context: Context, focus: Option[T], parent: Option[Cursor], env: Env) extends AbstractCursor {
        def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

        override def isNullable: Boolean = true
        override def asNullable: Result[Option[Cursor]] = {
          focus.traverse(elem => elemBuilder.build(context, elem, Some(this), env))
        }
      }

      new CursorBuilder[Option[T]] { outer =>
        val tpe = NullableType(elemBuilder.tpe)
        def build(context: Context, focus: Option[T], parent: Option[Cursor], env: Env): Result[Cursor] =
          OptionCursor(context.asType(tpe), focus, parent, env).rightIor
      }
    }

    implicit def listCursorBuiler[T](implicit elemBuilder: CursorBuilder[T]): CursorBuilder[List[T]] = {
      case class ListCursor(context: Context, focus: List[T], parent: Option[Cursor], env: Env) extends AbstractCursor {
        def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

        override def preunique: Result[Cursor] = {
          val listTpe = tpe.nonNull.list
          copy(context = context.asType(listTpe)).rightIor
        }

        override def isList: Boolean = true
        override def asList[C](factory: Factory[Cursor, C]): Result[C] = {
          focus.traverse(elem => elemBuilder.build(context, elem, Some(this), env)).map(_.to(factory))
        }
      }

      new CursorBuilder[List[T]] { outer =>
        val tpe = ListType(elemBuilder.tpe)
        def build(context: Context, focus: List[T], parent: Option[Cursor], env: Env): Result[Cursor] =
          ListCursor(context.asType(tpe), focus, parent, env).rightIor
      }
    }

    case class LeafCursor[T](context: Context, focus: T, encoder: Encoder[T], parent: Option[Cursor], env: Env) extends AbstractCursor {
      def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

      override def isLeaf: Boolean = true
      override def asLeaf: Result[Json] = encoder(focus).rightIor
    }

    def deriveLeafCursorBuilder[T](tpe0: Type)(implicit encoder: Encoder[T]): CursorBuilder[T] =
      new CursorBuilder[T] {
        val tpe = tpe0
        def build(context: Context, focus: T, parent: Option[Cursor], env: Env): Result[Cursor] =
          new LeafCursor(context.asType(tpe), focus, encoder, parent, env).rightIor
      }

    implicit def leafCursorBuilder[T](implicit encoder: Encoder[T]): CursorBuilder[T] =
      deriveLeafCursorBuilder(StringType)
  }

  abstract class PrimitiveCursor[T] extends AbstractCursor {
    val focus: T
    override def isLeaf: Boolean = true
  }
}
