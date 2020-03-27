// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.data.Ior
import cats.implicits._
import io.circe.Json

import QueryInterpreter.{ mkErrorResult, mkOneError }

/**
 * Indicates a position within an abstract data model during the interpretation
 * of a GraphQL query.
 */
trait Cursor {
  /** The value at the position represented by this `Cursor`. */
  def focus: Any
  /** The GraphQL type of the value at the position represented by this `Cursor`. */
  def tpe: Type

  /** Is the value at this `Cursor` of a scalar or enum type? */
  def isLeaf: Boolean

  /**
   * Yield the value at this `Cursor` rendered as Json if it is of a scalar or
   * enum type, an error or the left hand side otherwise.
   */
  def asLeaf: Result[Json]

  /** Is the value at this `Cursor` of a list type? */
  def isList: Boolean

  /**
   * Yield a list of `Cursor`s corresponding to the elements of the value at
   * this `Cursor` if it is of a list type, or an error or the left hand side
   * otherwise.
   */
  def asList: Result[List[Cursor]]

  /** Is the value at this `Cursor` of a nullable type? */
  def isNullable: Boolean

  /**
   * Yield an optional `Cursor`s corresponding to the value at this `Cursor` if
   * it is of a nullable type, or an error on the left hand side otherwise.  The
   * resulting `Cursor` will be present iff the current value is present in the
   * model.
   */
  def asNullable: Result[Option[Cursor]]

  /** Is the value at this `Cursor` narrowable to `subtpe`? */
  def narrowsTo(subtpe: TypeRef): Boolean

  /**
   * Yield a `Cursor` corresponding to the value at this `Cursor` narrowed to
   * type `subtpe`, or an error on the left hand side if such a narrowing is not
   * possible.
   */
  def narrow(subtpe: TypeRef): Result[Cursor]

  /** Does the value at this `Cursor` have a field named `fieldName`? */
  def hasField(fieldName: String): Boolean

  /**
   * Yield a `Cursor` corresponding to the value of the field `fieldName` of the
   * value at this `Cursor`, or an error on the left hand side if there is no
   * such field.
   */
  def field(fieldName: String): Result[Cursor]

  /** Does the value at this `Cursor` have an attribute named `attributeName`? */
  def hasAttribute(attributeName: String): Boolean

  /**
   * Yield the value of the attribute named `attributeName` of the value at this
   * `Cursor`, or an error on the left hand side if there is no such attribute.
   */
  def attribute(attributeName: String): Result[Any]

  /**
   * Does the possibly nullable value at this `Cursor` have an attributed named
   * `attributeName`?
   */
  def nullableHasField(fieldName: String): Boolean =
    if (isNullable)
      asNullable match {
        case Ior.Right(Some(c)) => c.nullableHasField(fieldName)
        case _ => false
      }
    else hasField(fieldName)

  /**
   * Yield a `Cursor` corresponding to the value of the possibly nullable field
   * `fieldName` of the value at this `Cursor`, or an error on the left hand
   * side if there is no such field.
   */
  def nullableField(fieldName: String): Result[Cursor] =
    if (isNullable)
      asNullable match {
        case Ior.Right(Some(c)) => c.nullableField(fieldName)
        case Ior.Right(None) => mkErrorResult(s"Expected non-null for field '$fieldName'")
        case Ior.Left(es) => es.leftIor
        case Ior.Both(es, _) => es.leftIor
      }
    else field(fieldName)

  /** Does the value at this `Cursor` have a field identified by the path `fns`? */
  def hasPath(fns: List[String]): Boolean = fns match {
    case Nil => true
    case fieldName :: rest =>
      nullableHasField(fieldName) && {
        nullableField(fieldName) match {
          case Ior.Right(c) =>
            !c.isList && c.hasPath(rest)
          case _ => false
        }
      }
  }

  /**
   * Yield a `Cursor` corresponding to the value of the field identified by path
   * `fns` starting from the value at this `Cursor`, or an error on the left
   * hand side if there is no such field.
   */
  def path(fns: List[String]): Result[Cursor] = fns match {
    case Nil => this.rightIor
    case fieldName :: rest =>
      nullableField(fieldName) match {
        case Ior.Right(c) => c.path(rest)
        case _ => mkErrorResult(s"Bad path")
      }
  }

  /**
   * Does the value at this `Cursor` generate a list along the path `fns`?
   *
   * `true` if `fns` is a valid path from the value at this `Cursor` and passes
   * through at least one field with a list type.
   */
  def hasListPath(fns: List[String]): Boolean = {
    def loop(c: Cursor, fns: List[String], seenList: Boolean): Boolean = fns match {
      case Nil => seenList
      case fieldName :: rest =>
        c.nullableHasField(fieldName) && {
          c.nullableField(fieldName) match {
            case Ior.Right(c) =>
              loop(c, rest, c.isList)
            case _ => false
          }
        }
    }

    loop(this, fns, false)
  }

  /**
   * Yield a list of `Cursor`s corresponding to the values generated by
   * following the path `fns` from the value at this `Cursor`, or an error on
   * the left hand side if there is no such path.
   */
  def listPath(fns: List[String]): Result[List[Cursor]] = fns match {
    case Nil => List(this).rightIor
    case fieldName :: rest =>
      if (isNullable)
        asNullable match {
          case Ior.Right(Some(c)) => c.listPath(fns)
          case Ior.Right(None) => Nil.rightIor
          case Ior.Left(es) => es.leftIor
          case Ior.Both(es, _) => es.leftIor
        }
      else if (isList)
        asList match {
          case Ior.Right(cs) => cs.flatTraverse(_.listPath(fns))
          case other => other
        }
      else
        field(fieldName) match {
          case Ior.Right(c) => c.listPath(rest)
          case Ior.Left(es) => es.leftIor
          case Ior.Both(es, _) => es.leftIor
        }
  }

  /**
   * Yield the list of values of the attribute generated by following the path
   * `fns` from the value at this `Cursor`, or an error on the left hand side if
   * there is no such path.
   */
  def attrListPath(fns: List[String]): Result[List[Any]] = fns match {
    case Nil => List(this).rightIor
    case List(attrName) if hasAttribute(attrName) =>
      attribute(attrName).map(List(_))
    case fieldName :: rest =>
      if (isNullable)
        asNullable match {
          case Ior.Right(Some(c)) => c.attrListPath(fns)
          case Ior.Right(None) => Nil.rightIor
          case Ior.Left(es) => es.leftIor
          case Ior.Both(es, _) => es.leftIor
        }
      else if (isList)
        asList match {
          case Ior.Right(cs) => cs.flatTraverse(_.attrListPath(fns))
          case other => other
        }
      else
        field(fieldName) match {
          case Ior.Right(c) => c.attrListPath(rest)
          case Ior.Left(es) => es.leftIor
          case Ior.Both(es, _) => es.leftIor
        }
  }
}

trait CursorBuilder[T] {
  def build(focus: T, tpe: Type): Result[Cursor]
}

object CursorBuilder {
  def apply[T](implicit cb: CursorBuilder[T]): CursorBuilder[T] = cb

  implicit val stringCursorBuilder: CursorBuilder[String] =
    new CursorBuilder[String] {
      def build(focus: String, tpe: Type): Result[Cursor] =
        new PrimitiveCursor(focus, tpe) {
          override def asLeaf: Result[Json] = Json.fromString(focus).rightIor
        }.rightIor
    }
  implicit val intCursorBuilder: CursorBuilder[Int] =
    new CursorBuilder[Int] {
      def build(focus: Int, tpe: Type): Result[Cursor] =
        new PrimitiveCursor(focus, tpe) {
          override def asLeaf: Result[Json] = Json.fromInt(focus).rightIor
        }.rightIor
    }
  implicit val longCursorBuilder: CursorBuilder[Long] =
    new CursorBuilder[Long] {
      def build(focus: Long, tpe: Type): Result[Cursor] =
        new PrimitiveCursor(focus, tpe) {
          override def asLeaf: Result[Json] = Json.fromLong(focus).rightIor
        }.rightIor
    }
  implicit val floatCursorBuilder: CursorBuilder[Float] =
    new CursorBuilder[Float] {
      def build(focus: Float, tpe: Type): Result[Cursor] =
        new PrimitiveCursor(focus, tpe) {
          override def asLeaf: Result[Json] =
            Json.fromFloat(focus).toRightIor(mkOneError(s"Unrepresentable float %focus"))
        }.rightIor
    }
  implicit val doubleCursorBuilder: CursorBuilder[Double] =
    new CursorBuilder[Double] {
      def build(focus: Double, tpe: Type): Result[Cursor] =
        new PrimitiveCursor(focus, tpe) {
          override def asLeaf: Result[Json] =
            Json.fromDouble(focus).toRightIor(mkOneError(s"Unrepresentable double %focus"))
        }.rightIor
    }
  implicit val booleanCursorBuilder: CursorBuilder[Boolean] =
    new CursorBuilder[Boolean] {
      def build(focus: Boolean, tpe: Type): Result[Cursor] =
        new PrimitiveCursor(focus, tpe) {
          override def asLeaf: Result[Json] = Json.fromBoolean(focus).rightIor
        }.rightIor
    }
  implicit def enumCursorBuilder[T <: Enumeration#Value]: CursorBuilder[T] =
    new CursorBuilder[T] {
      def build(focus: T, tpe: Type): Result[Cursor] =
        new PrimitiveCursor(focus, tpe) {
          override def asLeaf: Result[Json] = Json.fromString(focus.toString).rightIor
        }.rightIor
    }

  implicit def optionCursorBuiler[T](implicit elemBuilder: CursorBuilder[T]): CursorBuilder[Option[T]] =
    new CursorBuilder[Option[T]] {
      def build(focus0: Option[T], tpe0: Type): Result[Cursor] =
        new AbstractCursor[Option[T]] {
          def focus = focus0
          def tpe = tpe0

          override def isNullable: Boolean = true
          override def asNullable: Result[Option[Cursor]] = {
            val elemTpe = tpe.nonNull
            focus.traverse(elem => elemBuilder.build(elem, elemTpe))
          }
        }.rightIor
    }

  implicit def listCursorBuiler[T](implicit elemBuilder: CursorBuilder[T]): CursorBuilder[List[T]] =
    new CursorBuilder[List[T]] {
      def build(focus0: List[T], tpe0: Type): Result[Cursor] =
        new AbstractCursor[List[T]] {
          def focus = focus0
          def tpe = tpe0

          override def isList: Boolean = true
          override def asList: Result[List[Cursor]] = {
            val elemTpe = tpe.item
            focus.traverse(elem => elemBuilder.build(elem, elemTpe))
          }
        }.rightIor
    }
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

abstract class PrimitiveCursor[T](val focus: T, val tpe: Type) extends AbstractCursor[T] {
  override def isLeaf: Boolean = true
}
