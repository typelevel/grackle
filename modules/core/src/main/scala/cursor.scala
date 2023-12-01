// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
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
import scala.reflect.{classTag, ClassTag}

import cats.implicits._
import io.circe.Json
import org.tpolecat.typename.{TypeName, typeName}

import syntax._

/**
 * Indicates a position within an abstract data model during the interpretation
 * of a GraphQL query.
 */
trait Cursor {
  /** The parent of this `Cursor` */
  def parent: Option[Cursor]

  /** The value at the position represented by this `Cursor`. */
  def focus: Any

  /** The `Context` associated with this `Cursor`. */
  def context: Context

  /** The selection path from the root */
  def path: List[String] = context.path

  /** The selection path from the root modified by query aliases. */
  def resultPath: List[String] = context.resultPath

  /** The GraphQL type of the value at the position represented by this `Cursor`. */
  def tpe: Type = context.tpe

  /** Yields a copy of this `Cursor` with the supplied additional environment values. */
  def withEnv(env: Env): Cursor

  private[grackle] def env: Env

  /** Yields the value of the supplied environment key, if any. */
  def env[T: ClassTag](nme: String): Option[T] = env.get(nme).orElse(parent.flatMap(_.env(nme)))

  /** Yields the value of the supplied environment key, if any, or an error if none. */
  def envR[T: ClassTag: TypeName](nme: String): Result[T] =
    env(nme).toResultOrError(s"Key '$nme' of type ${typeName[T]} was not found in $this")

  /** Yields the cumulative environment defined at this `Cursor`. */
  def fullEnv: Env = parent.map(_.fullEnv).getOrElse(Env.empty).add(env)

  /** Does the environment at this `Cursor` contain a value for the supplied key? */
  def envContains(nme: String): Boolean = env.contains(nme) || parent.exists(_.envContains(nme))

  /**
   * Yield the value at this `Cursor` as a value of type `T` if possible,
   * an error or the left hand side otherwise.
   */
  def as[T: ClassTag: TypeName]: Result[T] =
    classTag[T].unapply(focus).toResultOrError(s"Expected value of type ${typeName[T]} for focus of type $tpe at path $path, found $focus")

  /** Is the value at this `Cursor` of a scalar or enum type? */
  def isLeaf: Boolean

  /**
   * Yield the value at this `Cursor` rendered as Json if it is of a scalar or
   * enum type, an error or the left hand side otherwise.
   */
  def asLeaf: Result[Json]

  /**
    * Yield a `Cursor` which can be used to evaluate the antecedant of a `Unique`
    * operation.
    */
  def preunique: Result[Cursor]

  /** Is the value at this `Cursor` of a list type? */
  def isList: Boolean

  /**
   * Yield a list of `Cursor`s corresponding to the elements of the value at
   * this `Cursor` if it is of a list type, or an error or the left hand side
   * otherwise.
   */
  final def asList: Result[List[Cursor]] = asList(List)

  /**
   * Yield a collection of `Cursor`s corresponding to the elements of the value at
   * this `Cursor` if it is of a list type, or an error or the left hand side
   * otherwise.
   */
  def asList[C](factory: Factory[Cursor, C]): Result[C]

  /**
    * Yields the number of elements of this `Cursor` if it is of a list type, or an
    * error otherwise.
    */
  def listSize: Result[Int]

  /** Is the value at this `Cursor` of a nullable type? */
  def isNullable: Boolean

  /**
   * Yield an optional `Cursor`s corresponding to the value at this `Cursor` if
   * it is of a nullable type, or an error on the left hand side otherwise.  The
   * resulting `Cursor` will be present iff the current value is present in the
   * model.
   */
  def asNullable: Result[Option[Cursor]]

  /**
    * Yields whether or not this `Cursor` is defined if it is of a nullable type,
    * or an error otherwise.
    */
  def isDefined: Result[Boolean]

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
  def field(fieldName: String, resultName: Option[String]): Result[Cursor]

  /**
   * Yield the value of the field `fieldName` of this `Cursor` as a value of
   * type `T` if possible, an error or the left hand side otherwise.
   */
  def fieldAs[T: ClassTag : TypeName](fieldName: String): Result[T] =
    field(fieldName, None).flatMap(_.as[T])

  /** True if this cursor is nullable and null, false otherwise. */
  def isNull: Boolean =
    isNullable && (asNullable match {
      case Result.Success(None) => true
      case _ => false
    })

  /**
   * Does the possibly nullable value at this `Cursor` have a field named
   * `fieldName`?
   */
  def nullableHasField(fieldName: String): Boolean =
    if (isNullable)
      asNullable match {
        case Result.Success(Some(c)) => c.nullableHasField(fieldName)
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
      asNullable.flatMap {
        case Some(c) => c.nullableField(fieldName)
        case None => Result.internalError(s"Expected non-null for field '$fieldName'")
      }
    else field(fieldName, None)

  /** Does the value at this `Cursor` have a field identified by the path `fns`? */
  def hasPath(fns: List[String]): Boolean = fns match {
    case Nil => true
    case fieldName :: rest =>
      nullableHasField(fieldName) && {
        nullableField(fieldName) match {
          case Result.Success(c) =>
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
    case Nil => this.success
    case fieldName :: rest =>
      nullableField(fieldName) match {
        case Result.Success(c) => c.path(rest)
        case _ => Result.internalError(s"Bad path")
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
            case Result.Success(c) =>
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
    case Nil => List(this).success
    case fieldName :: rest =>
      if (isNullable)
        asNullable.flatMap {
          case Some(c) => c.listPath(fns)
          case None => Nil.success
        }
      else if (isList)
        asList.flatMap(_.flatTraverse(_.listPath(fns)))
      else
        field(fieldName, None).flatMap(_.listPath(rest))
  }

  /**
   * Yield a list of `Cursor`s corresponding to the values generated by
   * following the path `fns` from the value at this `Cursor`, or an error on
   * the left hand side if there is no such path. If the field at the end
   * of the path is a list then yield the concatenation of the lists of
   * cursors corresponding to the field elements.
   */
  def flatListPath(fns: List[String]): Result[List[Cursor]] =
    listPath(fns).flatMap(cs => cs.flatTraverse(c => if (c.isList) c.asList else List(c).success))
}

object Cursor {

  def flatten(c: Cursor): Result[List[Cursor]] =
    if(c.isList) c.asList.flatMap(flatten)
    else if(c.isNullable) c.asNullable.flatMap(oc => flatten(oc.toList))
    else List(c).success

  def flatten(cs: List[Cursor]): Result[List[Cursor]] =
    cs.flatTraverse(flatten)

  /** Abstract `Cursor` providing default implementation of most methods. */
  abstract class AbstractCursor extends Cursor {
    def isLeaf: Boolean = false

    def asLeaf: Result[Json] =
      Result.internalError(s"Expected Scalar type, found $tpe for focus ${focus} at ${context.path.reverse.mkString("/")}")

    def preunique: Result[Cursor] =
      Result.internalError(s"Expected List type, found $focus for ${tpe.nonNull.list}")

    def isList: Boolean = false

    def asList[C](factory: Factory[Cursor, C]): Result[C] =
      Result.internalError(s"Expected List type, found $tpe")

    def listSize: Result[Int] =
      Result.internalError(s"Expected List type, found $tpe")

    def isNullable: Boolean = false

    def asNullable: Result[Option[Cursor]] =
      Result.internalError(s"Expected Nullable type, found $focus for $tpe")

    def isDefined: Result[Boolean] =
      Result.internalError(s"Expected Nullable type, found $focus for $tpe")

    def narrowsTo(subtpe: TypeRef): Boolean = false

    def narrow(subtpe: TypeRef): Result[Cursor] =
      Result.internalError(s"Focus ${focus} of static type $tpe cannot be narrowed to $subtpe")

    def hasField(fieldName: String): Boolean = false

    def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      Result.internalError(s"No field '$fieldName' for type ${tpe.underlying}")
  }

  /** Proxy `Cursor` which delegates most methods to an underlying `Cursor`.. */
  class ProxyCursor(underlying: Cursor) extends Cursor {
    def context: Context = underlying.context

    def env: Env = underlying.env

    def focus: Any = underlying.focus

    def parent: Option[Cursor] = underlying.parent

    def withEnv(env: Env): Cursor = underlying.withEnv(env)

    def isLeaf: Boolean = underlying.isLeaf

    def asLeaf: Result[Json] = underlying.asLeaf

    def preunique: Result[Cursor] = underlying.preunique

    def isList: Boolean = underlying.isList

    def asList[C](factory: Factory[Cursor, C]): Result[C] = underlying.asList(factory)

    def listSize: Result[Int] = underlying.listSize

    def isNullable: Boolean = underlying.isNullable

    def asNullable: Result[Option[Cursor]] = underlying.asNullable

    def isDefined: Result[Boolean] = underlying.isDefined

    def narrowsTo(subtpe: TypeRef): Boolean = underlying.narrowsTo(subtpe)

    def narrow(subtpe: TypeRef): Result[Cursor] = underlying.narrow(subtpe)

    def hasField(fieldName: String): Boolean = underlying.hasField(fieldName)

    def field(fieldName: String, resultName: Option[String]): Result[Cursor] = underlying.field(fieldName, resultName)
  }

  /** Empty `Cursor` with no content */
  case class EmptyCursor(context: Context, parent: Option[Cursor], env: Env) extends AbstractCursor {
    def focus: Any = Result.internalError(s"Empty cursor has no focus")
    def withEnv(env0: Env): EmptyCursor = copy(env = env.add(env0))
  }

  /**
   * Proxy list cursor which substitutes an alternative set of elements.
   *
   * Typically used as the result of a `TransformCursor` operation
   */
  case class ListTransformCursor(underlying: Cursor, newSize: Int, newElems: Seq[Cursor]) extends ProxyCursor(underlying) {
    override def withEnv(env: Env): Cursor = new ListTransformCursor(underlying.withEnv(env), newSize, newElems)
    override lazy val listSize: Result[Int] = newSize.success
    override def asList[C](factory: Factory[Cursor, C]): Result[C] =
      factory.fromSpecific(newElems).success
  }

  /** Proxy `Cursor` which always yields a `NullCursor` for fields of the underlying cursor */
  case class NullFieldCursor(underlying: Cursor) extends ProxyCursor(underlying) {
    override def withEnv(env: Env): Cursor = new NullFieldCursor(underlying.withEnv(env))
    override def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      underlying.field(fieldName, resultName).map(NullCursor(_))
  }

  /** Proxy `Cursor` which always yields null */
  case class NullCursor(underlying: Cursor) extends ProxyCursor(underlying) {
    override def withEnv(env: Env): Cursor = new NullCursor(underlying.withEnv(env))
    override def isDefined: Result[Boolean] = false.success
    override def asNullable: Result[Option[Cursor]] = None.success
  }

  case class DeferredCursor(context: Context, parent: Option[Cursor], env: Env, deferredPath: List[String], mkCursor: (Context, Cursor) => Result[Cursor]) extends AbstractCursor {
    def focus: Any = Result.internalError(s"Empty cursor has no focus")
    def withEnv(env0: Env): DeferredCursor = copy(env = env.add(env0))

    override def hasField(fieldName: String): Boolean = fieldName == deferredPath.head

    override def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      if(fieldName != deferredPath.head) Result.internalError(s"No field '$fieldName' for type ${tpe.underlying}")
      else
        for {
          fieldContext <- context.forField(fieldName, resultName)
          cursor       <- if(path.sizeCompare(1) > 0) DeferredCursor(fieldContext, Some(this), env, deferredPath.tail, mkCursor).success
                          else mkCursor(fieldContext, this)
        } yield cursor
  }

  object DeferredCursor {
    def apply(path: Path, mkCursor: (Context, Cursor) => Result[Cursor]): Cursor =
      DeferredCursor(Context(path.rootTpe), None, Env.empty, path.path, mkCursor)
  }
}

/**
  * Context represents a position in the output tree in terms of,
  * 1) the path through the schema to the position
  * 2) the path through the schema with query aliases applied
  * 3) the type of the element at the position
  */
case class Context(
  rootTpe: Type,
  path: List[String],
  resultPath: List[String],
  typePath: List[Type]
) {
  lazy val tpe: Type = typePath.headOption.getOrElse(rootTpe)

  def asType(tpe: Type): Context = {
    typePath match {
      case Nil => copy(rootTpe = tpe)
      case _ :: tl => copy(typePath = tpe :: tl)
    }
  }

  def isRoot: Boolean = path.isEmpty

  def parent: Option[Context] =
    if(path.isEmpty) None
    else Some(copy(path = path.tail, resultPath = resultPath.tail, typePath = typePath.tail))

  def forField(fieldName: String, resultName: String): Result[Context] =
    tpe.underlyingField(fieldName).map { fieldTpe =>
      copy(path = fieldName :: path, resultPath = resultName :: resultPath, typePath = fieldTpe :: typePath)
    }.toResult(s"No field '$fieldName' for type ${tpe.underlying}")

  def forField(fieldName: String, resultName: Option[String]): Result[Context] =
    tpe.underlyingField(fieldName).map { fieldTpe =>
      copy(path = fieldName :: path, resultPath = resultName.getOrElse(fieldName) :: resultPath, typePath = fieldTpe :: typePath)
    }.toResult(s"No field '$fieldName' for type ${tpe.underlying}")

  def forPath(path1: List[String]): Result[Context] =
    path1 match {
      case Nil => this.success
      case hd :: tl => forField(hd, hd).flatMap(_.forPath(tl))
    }

  def forFieldOrAttribute(fieldName: String, resultName: Option[String]): Context = {
    val fieldTpe = tpe.underlyingField(fieldName).getOrElse(ScalarType.AttributeType)
    copy(path = fieldName :: path, resultPath = resultName.getOrElse(fieldName) :: resultPath, typePath = fieldTpe :: typePath)
  }

  override def equals(other: Any): Boolean =
    other match {
      case Context(oRootTpe, oPath, oResultPath, _) =>
        rootTpe =:= oRootTpe && resultPath == oResultPath && path == oPath
      case _ => false
    }

  override def hashCode(): Int = resultPath.hashCode
}

object Context {
  def apply(rootTpe: Type, fieldName: String, resultName: Option[String]): Option[Context] = {
    for {
      tpe <- rootTpe.underlyingField(fieldName)
    } yield new Context(rootTpe, List(fieldName), List(resultName.getOrElse(fieldName)), List(tpe))
  }

  def apply(rootTpe: Type): Context = Context(rootTpe, Nil, Nil, Nil)

  def apply(path: Path): Result[Context] =
    path.path.foldLeftM(Context(path.rootTpe, Nil, Nil, Nil)) { case (acc, elem) =>
      acc.forField(elem, None)
    }
}

/**
 * An environment for elaboration or execution of a GraphQL query.
 */
sealed trait Env {
  def add[T](items: (String, T)*): Env
  def add(env: Env): Env
  def contains(name: String): Boolean
  def get[T: ClassTag](name: String): Option[T]
  def isEmpty: Boolean

  def getR[A: ClassTag: TypeName](name: String): Result[A] =
    get[A](name).toResultOrError(s"Key '$name' of type ${typeName[A]} was not found in $this")

  def addFromQuery(query: Query): Env =
    query match {
      case Query.Environment(childEnv, child) =>
        add(childEnv).addFromQuery(child)
      case _ => this
    }
}

object Env {
  def empty: Env = EmptyEnv

  def apply[T](items: (String, T)*): Env = NonEmptyEnv(Map(items: _*))

  case object EmptyEnv extends Env {
    def add[T](items: (String, T)*): Env = NonEmptyEnv(Map(items: _*))
    def add(env: Env): Env = env
    def contains(name: String): Boolean = false
    def get[T: ClassTag](name: String): Option[T] = None
    def isEmpty: Boolean = true
  }

  case class NonEmptyEnv(elems: Map[String, Any]) extends Env {
    def add[T](items: (String, T)*): Env = NonEmptyEnv(elems++items)
    def add(other: Env): Env = other match {
      case EmptyEnv => this
      case NonEmptyEnv(elems0) => NonEmptyEnv(elems++elems0)
    }
    def contains(name: String): Boolean = elems.contains(name)
    def get[T: ClassTag](name: String): Option[T] =
      elems.get(name).flatMap(classTag[T].unapply)
    def isEmpty: Boolean = false
  }
}
