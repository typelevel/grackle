// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import scala.reflect.{classTag, ClassTag}

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

  /** The selection path from the root */
  def path: List[String]

  /**
   * Yield the value at this `Cursor` as a value of type `T` if possible,
   * an error or the left hand side otherwise.
   */
  def as[T: ClassTag]: Result[T] =
    cast[T](focus).toRightIor(mkOneError(s"Expected value of type ${classTag[T]} for focus of type $tpe, found $focus"))

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

  /**
   * Yield the value of the field `fieldName` of this `Cursor` as a value of
   * type `T` if possible, an error or the left hand side otherwise.
   */
  def fieldAs[T: ClassTag](fieldName: String): Result[T] =
    field(fieldName).flatMap(_.as[T])

  /** Does the value at this `Cursor` have an attribute named `attributeName`? */
  def hasAttribute(attributeName: String): Boolean

  /**
   * Yield the value of the attribute named `attributeName` of the value at this
   * `Cursor`, or an error on the left hand side if there is no such attribute.
   */
  def attribute(attributeName: String): Result[Any]

  /**
   * Yield the value of the attribute `attributeName` of this `Cursor` as a
   * value of type `T` if possible, an error or the left hand side otherwise.
   */
  def attributeAs[T: ClassTag](attributeName: String): Result[T] =
    attribute(attributeName).flatMap { attr =>
      cast[T](attr).toRightIor(mkOneError(s"Expected value of type ${classTag[T]} for field '$attributeName' found '$attr' of type ${attr.getClass.getName}"))
    }

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
   * Yield a list of `Cursor`s corresponding to the values generated by
   * following the path `fns` from the value at this `Cursor`, or an error on
   * the left hand side if there is no such path. If the field at the end
   * of the path is a list then yield the concatenation of the lists of
   * cursors corresponding to the field elements.
   */
  def flatListPath(fns: List[String]): Result[List[Cursor]] =
    listPath(fns).flatMap(cs => cs.flatTraverse(c => if (c.isList) c.asList else List(c).rightIor))

  /**
   * Yield the list of values of the attribute generated by following the path
   * `fns` from the value at this `Cursor`, or an error on the left hand side if
   * there is no such path.
   */
  def attrListPath(fns: List[String]): Result[List[Any]] = {
    fns match {
      case Nil => mkOneError("Unresolved path").leftIor
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

  private def cast[T: ClassTag](x: Any): Option[T] = {
    val clazz = classTag[T].runtimeClass
    if (
      clazz.isInstance(x) ||
      (clazz == classOf[Int] && x.isInstanceOf[java.lang.Integer]) ||
      (clazz == classOf[Boolean] && x.isInstanceOf[java.lang.Boolean]) ||
      (clazz == classOf[Long] && x.isInstanceOf[java.lang.Long]) ||
      (clazz == classOf[Double] && x.isInstanceOf[java.lang.Double]) ||
      (clazz == classOf[Byte] && x.isInstanceOf[java.lang.Byte]) ||
      (clazz == classOf[Short] && x.isInstanceOf[java.lang.Short]) ||
      (clazz == classOf[Char] && x.isInstanceOf[java.lang.Character]) ||
      (clazz == classOf[Float] && x.isInstanceOf[java.lang.Float]) ||
      (clazz == classOf[Unit] && x.isInstanceOf[scala.runtime.BoxedUnit])
    )
      Some(x.asInstanceOf[T])
    else
      None
  }
}
