// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package doobie

import scala.util.matching.Regex

import cats.data.Chain
import cats.effect.Bracket
import cats.implicits._
import _root_.doobie.Transactor
import _root_.doobie.implicits._
import io.chrisdavenport.log4cats.Logger
import io.circe.Json

import DoobieMapping._
import Predicate._
import Query._
import QueryInterpreter.{ mkErrorResult, ProtoJson }

class DoobieQueryInterpreter[F[_]](
  mapping: DoobieMapping,
  xa: Transactor[F],
  logger: Logger[F]
) (override implicit val F: Bracket[F, Throwable]) extends QueryInterpreter[F] {

  def runRootValue(query: Query, rootTpe: Type): F[Result[ProtoJson]] =
    query match {
      case PossiblyRenamedSelect(Select(fieldName, _, child), resultName) =>
        val fieldTpe = rootTpe.field(fieldName)
        val mapped = mapping.mapQuery(child, fieldTpe)

        for {
          table <- logger.info(s"fetch(${mapped.fragment})") *> mapped.fetch.transact(xa)
        } yield runValue(Wrap(resultName, child), fieldTpe, DoobieCursor(mapped.rootCursorType(fieldTpe), table, mapped))

      case _ => mkErrorResult(s"Bad root query '${query.render}' in DoobieQueryInterpreter").pure[F]
    }

  override def runRootValues(queries: List[(Query, Type)]): F[(Chain[Json], List[ProtoJson])] = {
    // TODO: combine sibling queries here
    super.runRootValues(queries)
  }
}

object DoobiePredicate {
  def paths(pred: Predicate): List[Path] = {
    def path[T](term: Term[T]): List[Path] =
      term match {
        case p: Path => List(p)
        case _ => Nil
      }
    pred match {
      case And(x, y) => paths(x) ++ paths(y)
      case Or(x, y) => paths(x) ++ paths(y)
      case Not(x) => paths(x)
      case Eql(x, y) => path(x) ++ path(y)
      case Contains(x, y) => path(x) ++ path(y)
      case Lt(x, y) => path(x) ++ path(y)
      case Matches(x, _) => path(x)
      case Like(x, _, _) => path(x)
      case _ => Nil
    }
  }

  def isField(p: Path): Boolean =
    p match {
      case FieldPath(_) => true
      case _ => false
    }

  def likeToRegex(pattern: String, caseInsensitive: Boolean): Regex = {
    val csr = ("^"+pattern.replace("%", ".*").replace("_", ".")+"$")
    (if (caseInsensitive) s"(?i:$csr)" else csr).r
  }

  case class Like(x: Term[String], pattern: String, caseInsensitive: Boolean) extends Prop {
    lazy val r = likeToRegex(pattern, caseInsensitive)

    def apply(c: Cursor): Boolean =
      x(c) match {
        case List(x0) => r.matches(x0)
        case _ => false
      }
  }
}

case class DoobieCursor(val tpe: Type, val focus: Any, mapped: MappedQuery) extends Cursor {
  def asTable: Result[Table] = focus match {
    case table@((_: Row) :: _ | Nil) => table.asInstanceOf[Table].rightIor
    case _ => mkErrorResult(s"Not a table")
  }

  def isUnstructured(tpe: Type): Boolean =
    tpe match {
      case NullableType(tpe) => isUnstructured(tpe)
      case ListType(tpe) => isUnstructured(tpe)
      case TypeRef(_, _) => tpe.dealias.isLeaf
      case _: ScalarType => true
      case _: EnumType => true
      case _ => false
    }

  def isLeaf: Boolean = tpe.isLeaf

  def asLeaf: Result[Json] =
    focus match {
      case s: String => Json.fromString(s).rightIor
      case i: Int => Json.fromInt(i).rightIor
      case d: Double => Json.fromDouble(d) match {
          case Some(j) => j.rightIor
          case None => mkErrorResult(s"Unrepresentable double %d")
        }
      case b: Boolean => Json.fromBoolean(b).rightIor

      // This means we are looking at a column with no value because it's the result of a failed
      // outer join. This is an implementation error.
      case Row.FailedJoin => sys.error("Unhandled failed join.")

      case _ => mkErrorResult("Not a leaf")
    }

  def isList: Boolean =
    tpe match {
      case ListType(_) => true
      case _ => false
    }

  def asList: Result[List[Cursor]] =
    if (!tpe.isList) mkErrorResult(s"Not a list: $tpe")
    else {
      val itemTpe = tpe.item.dealias
      asTable.map { table =>

        // The object mapping for `tpe`.
        val objectMapping: ObjectMapping =
          mapped.mapping.objectMapping(itemTpe).getOrElse(sys.error(s"No ObjectMapping for $itemTpe"))

        // If this mapping is a list of child objects then its fields came from an outer join. If
        // there are no children then all keys defined in the mapping will have the `FailedJoin`
        // value.
        val isEmpty: Boolean =
          objectMapping.key.forall { cr =>
            val ix = mapped.index(cr)
            table.forall(r => r(ix) == Row.FailedJoin)
          }

        // Sanity check: isEmpty implies that we had zero rows, or one row with failed joins.
        if (isEmpty)
          assert(table.length <= 1)

        // Done!
        if (isEmpty) Nil
        else mapped.group(table, itemTpe).map(table => copy(tpe = itemTpe, focus = table))
      }
    }

  def isNullable: Boolean =
    tpe match {
      case NullableType(_) => true
      case _ => false
    }

  def asNullable: Result[Option[Cursor]] =
    (tpe, focus) match {
      case (NullableType(_), None) => None.rightIor
      case (NullableType(tpe), Some(v)) => Some(copy(tpe = tpe, focus = v)).rightIor
      case (NullableType(tpe), _) => Some(copy(tpe = tpe)).rightIor // non-nullable column as nullable schema type (ok)
      case _ => mkErrorResult("Not nullable")
    }

  def narrowsTo(subtpe: TypeRef): Boolean = false

  def narrow(subtpe: TypeRef): Result[Cursor] =
    mkErrorResult(s"Cannot narrow $tpe to $subtpe")

  def hasField(fieldName: String): Boolean = {
    val fieldTpe = tpe.field(fieldName)
    if (fieldTpe.isLeaf)
      mapped.hasField(tpe, fieldName)
    else
      mapped.hasSubobject(fieldTpe.underlyingObject)
  }

  def field(fieldName: String): Result[Cursor] = {
    val fieldTpe = tpe.field(fieldName)
    if (isUnstructured(fieldTpe))
      asTable.map(table => copy(tpe = fieldTpe, focus = mapped.selectField(table.head, tpe, fieldName)))
    else
      copy(tpe = fieldTpe).rightIor
  }

  def hasAttribute(attributeName: String): Boolean =
    mapped.hasAttribute(tpe, attributeName)

  def attribute(attributeName: String): Result[Any] =
    asTable.map(table => mapped.selectAttribute(table.head, tpe, attributeName))
}
