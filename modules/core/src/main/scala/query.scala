// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.runtime.ScalaRunTime
import scala.util.matching.Regex

import cats.{ Monad, Monoid }
import cats.data.{ Chain, Ior, IorT, NonEmptyChain }
import cats.implicits._
import io.circe.Json
import io.circe.literal.JsonStringContext

import ComponentMapping.NoMapping
import Query._

sealed trait Query {
  def ~(query: Query): Query = (this, query) match {
    case (Group(hd), Group(tl)) => Group(hd ++ tl)
    case (hd, Group(tl)) => Group(hd :: tl)
    case (Group(hd), tl) => Group(hd :+ tl)
    case (hd, tl) => Group(List(hd, tl))
  }

  def render: String
}

object Query {
  case class Select(name: String, args: List[Binding], child: Query = Empty) extends Query {
    def render = {
      val rargs = if(args.isEmpty) "" else s"(${args.map(_.render).mkString(", ")})"
      val rchild = if(child == Empty) "" else s" { ${child.render} }"
      s"$name$rargs$rchild"
    }
  }
  case class Group(queries: List[Query]) extends Query {
    def render = queries.map(_.render).mkString(", ")
  }
  case class Unique(pred: Predicate, child: Query) extends Query {
    def render = s"<unique> $child"
  }
  case class Filter(pred: Predicate, child: Query) extends Query {
    def render = s"<filter> $child"
  }
  case object Empty extends Query {
    def render = ""
  }

  sealed trait Binding {
    def name: String
    type T
    val value: T

    def render: String
  }
  object Binding {
    case class StringBinding(name: String, value: String) extends Binding {
      type T = String
      def render = s"""$name: "$value""""
    }

    def toMap(bindings: List[Binding]): Map[String, Any] =
      bindings.map(b => (b.name, b.value)).toMap
  }
}

trait Predicate extends Product with (Cursor => Boolean) {
  override def toString = ScalaRunTime._toString(this)
}

object Predicate {
  object StringScalarFocus {
    def unapply(c: Cursor): Option[String] =
      c.focus match {
        case s: String => Some(s)
        case _ => None
      }
  }

  case class FieldEquals(fieldName: String, value: String) extends Predicate {
    def apply(c: Cursor): Boolean =
      c.field(fieldName, Map.empty[String, Any]) match {
        case Ior.Right(StringScalarFocus(`value`)) => true
        case _ => false
      }
  }

  case class FieldMatches(fieldName: String, r: Regex) extends Predicate {
    def apply(c: Cursor): Boolean =
      c.field(fieldName, Map.empty[String, Any]) match {
        case Ior.Right(StringScalarFocus(value)) => r.matches(value)
        case _ => false
      }
  }

  case class AttrEquals(attrName: String, value: String) extends Predicate {
    def apply(c: Cursor): Boolean =
      c.attribute(attrName) match {
        case Ior.Right(`value`) => true
        case _ => false
      }
  }

  case class AttrMatches(attrName: String, r: Regex) extends Predicate {
    def apply(c: Cursor): Boolean =
      c.attribute(attrName) match {
        case Ior.Right(value: String) => r.matches(value)
        case _ => false
      }
  }
}

abstract class QueryInterpreter[F[_]]
  (val schema: Schema, val mapping: ComponentMapping[F] = NoMapping[F])
  (implicit val F: Monad[F]) {

  import QueryInterpreter.{ complete, mkErrorResult, ProtoJson }

  def run(query: Query): F[Json] =
    runRoot(query).map(QueryInterpreter.mkResponse)

  def runRoot(query: Query): F[Result[Json]] =
    query match {
      case Select(fieldName, _, _) =>
        (for {
          pvalue <- IorT(runRootValue(query))
          value  <- IorT(complete(pvalue, mapping))
        } yield Json.obj((fieldName, value))).value

      case _ =>
        mkErrorResult(s"Bad query: $query").pure[F]
    }

  def elaborateSelect(tpe: Type, query: Select): Result[Query]

  def elaborateSelects(query: Query): Result[Query] = {
    def loop(query: Query, tpe: Type, isRoot: Boolean): Result[Query] = {
      if (!mapping.isNoMapping && !isRoot && mapping.interpreterForType(tpe.dealias).fold(true)(_ ne this))
        query.rightIor
      else
        query match {
          case Select(fieldName, args, child) =>
            for {
              elaboratedChild  <- loop(child, tpe.field(fieldName), false)
              elaboratedParent <- elaborateSelect(tpe, Select(fieldName, args, elaboratedChild))
            } yield elaboratedParent

          case Group(queries)      => queries.traverse(q => loop(q, tpe, false)).map(Group(_))
          case Unique(pred, child) => loop(child, tpe.nonNull, false).map(c => Unique(pred, c))
          case Filter(pred, child) => loop(child, tpe.item, false).map(c => Filter(pred, c))
          case Empty               => Empty.rightIor
        }
    }

    loop(query, schema.queryType, true)
  }

  def runRootValue(query: Query): F[Result[ProtoJson]]

  def runRootValues(queries: List[Query]): F[(Chain[Json], List[ProtoJson])] =
    queries.traverse(runRootValue).map { rs =>
      (rs.foldLeft((Chain.empty[Json], List.empty[ProtoJson])) {
        case ((errors, elems), elem) =>
          elem match {
            case Ior.Left(errs) => (errs.toChain ++ errors, ProtoJson.fromJson(Json.Null) :: elems)
            case Ior.Right(elem) => (errors, elem :: elems)
            case Ior.Both(errs, elem) => (errs.toChain ++ errors, elem :: elems)
          }
      }).map(_.reverse)
    }

  def runFields(query: Query, tpe: Type, cursor: Cursor): Result[List[(String, ProtoJson)]] = {
    (query, tpe.dealias) match {
      case (sel@Select(fieldName, _, _), NullableType(tpe)) =>
        cursor.asNullable.sequence.map { rc =>
          for {
            c      <- rc
            fields <- runFields(sel, tpe, c)
          } yield fields
        }.getOrElse(List((fieldName, ProtoJson.fromJson(Json.Null))).rightIor)

      case (Select(fieldName, bindings, child), tpe) =>
        if (!cursor.hasField(fieldName))
          List((fieldName, ProtoJson.deferred(cursor, tpe, fieldName, child))).rightIor
        else
          for {
            c     <- cursor.field(fieldName, Binding.toMap(bindings))
            value <- runValue(child, tpe.field(fieldName), c)
          } yield List((fieldName, value))

      case (Group(siblings), _) =>
        siblings.flatTraverse(query => runFields(query, tpe, cursor))

      case _ =>
        mkErrorResult(s"failed: { ${query.render} } ${tpe.shortString}")
    }
  }

  def runValue(query: Query, tpe: Type, cursor: Cursor): Result[ProtoJson] = {
    (query, tpe.dealias) match {
      case (Unique(pred, child), _) if cursor.isList =>
        cursor.asList.map(_.filter(pred)).flatMap(lc =>
          lc match {
            case List(c) => runValue(child, tpe.nonNull, c)
            case Nil if tpe.isNullable => ProtoJson.fromJson(Json.Null).rightIor
            case Nil => mkErrorResult(s"No match")
            case _ => mkErrorResult(s"Multiple matches")
          }
        )

      case (_, NullableType(tpe)) =>
        cursor.asNullable.sequence.map { rc =>
          for {
            c     <- rc
            value <- runValue(query, tpe, c)
          } yield value
        }.getOrElse(ProtoJson.fromJson(Json.Null).rightIor)

      case (Filter(pred, child), ListType(tpe)) =>
        cursor.asList.map(_.filter(pred)).flatMap(lc =>
          lc.traverse(c => runValue(child, tpe, c)).map(ProtoJson.fromValues)
        )

      case (_, ListType(tpe)) =>
        cursor.asList.flatMap(lc =>
          lc.traverse(c => runValue(query, tpe, c)).map(ProtoJson.fromValues)
        )

      case (_, (_: ScalarType) | (_: EnumType)) => cursor.asLeaf.map(ProtoJson.fromJson)

      case (_, (_: ObjectType) | (_: InterfaceType)) =>
        runFields(query, tpe, cursor).map(ProtoJson.fromFields)

      case _ =>
        mkErrorResult(s"Unknown type $tpe")
    }
  }
}

object QueryInterpreter {
  type ProtoJson <: AnyRef

  object ProtoJson {
    private[QueryInterpreter] case class DeferredJson(cursor: Cursor, tpe: Type, fieldName: String, query: Query)
    private[QueryInterpreter] case class ProtoObject(fields: List[(String, ProtoJson)])
    private[QueryInterpreter] case class ProtoArray(elems: List[ProtoJson])

    def deferred(cursor: Cursor, tpe: Type, fieldName: String, query: Query): ProtoJson =
      wrap(DeferredJson(cursor, tpe, fieldName, query))

    def fromJson(value: Json): ProtoJson = wrap(value)

    def fromFields(fields: List[(String, ProtoJson)]): ProtoJson =
      if(fields.forall(_._2.isInstanceOf[Json]))
        wrap(Json.fromFields(fields.asInstanceOf[List[(String, Json)]]))
      else
        wrap(ProtoObject(fields))

    def fromValues(elems: List[ProtoJson]): ProtoJson =
      if(elems.forall(_.isInstanceOf[Json]))
        wrap(Json.fromValues(elems.asInstanceOf[List[Json]]))
      else
        wrap(ProtoArray(elems))

    private def wrap(j: AnyRef): ProtoJson = j.asInstanceOf[ProtoJson]
  }

  import ProtoJson._

  def complete[F[_]: Monad](pj: ProtoJson, mapping: ComponentMapping[F]): F[Result[Json]] =
    completeAll(List(pj), mapping).map {
      case (errors, List(value)) =>
        NonEmptyChain.fromChain(errors) match {
          case Some(errors) => Ior.Both(errors, value)
          case None => value.rightIor
        }
    }

  def completeAll[F[_]: Monad](pjs: List[ProtoJson], mapping: ComponentMapping[F]): F[(Chain[Json], List[Json])] = {
    def gatherDeferred(pj: ProtoJson): List[DeferredJson] = {
      @tailrec
      def loop(pending: Chain[ProtoJson], acc: List[DeferredJson]): List[DeferredJson] =
        pending.uncons match {
          case None => acc
          case Some((hd, tl)) => hd match {
            case _: Json             => loop(tl, acc)
            case d: DeferredJson     => loop(tl, d :: acc)
            case ProtoObject(fields) => loop(Chain.fromSeq(fields.map(_._2)) ++ tl, acc)
            case ProtoArray(elems)   => loop(Chain.fromSeq(elems) ++ tl, acc)
          }
        }

      pj match {
        case _: Json => Nil
        case _ => loop(Chain.one(pj), Nil)
      }
    }

    def scatterResults(pj: ProtoJson, subst: mutable.Map[DeferredJson, Json]): Json = {
      def loop(pj: ProtoJson): Json =
        pj match {
          case p: Json         => p
          case d: DeferredJson => subst(d)
          case ProtoObject(fields) =>
            val values = fields.map(f => loop(f._2))
            Json.fromFields(fields.iterator.map(_._1).zip(values).toList)
          case ProtoArray(elems) =>
            val elems0 = elems.map(loop)
            Json.fromValues(elems0)
        }

      loop(pj)
    }

    val collected = pjs.flatMap(gatherDeferred)

    val (good, bad, errors0) =
      collected.foldLeft((List.empty[(DeferredJson, QueryInterpreter[F], Query)], List.empty[DeferredJson], Chain.empty[Json])) {
        case ((good, bad, errors), d@DeferredJson(cursor, tpe, fieldName, query)) =>
          mapping.subobject(tpe, fieldName) match {
            case Some(mapping.Subobject(submapping, subquery)) =>
              subquery(cursor, query) match {
                case Ior.Right(query) =>
                  ((d, submapping.interpreter, query) :: good, bad, errors)
                case Ior.Both(errs, query) =>
                  ((d, submapping.interpreter, query) :: good, bad, errs.toChain ++ errors)
                case Ior.Left(errs) =>
                  (good, d :: bad, errs.toChain ++ errors)
              }
            case None =>
              (good, d :: bad, mkError(s"Bad query: ${tpe.shortString} $fieldName ${query.render}") +: errors)
          }
      }

    val grouped = good.groupMap(_._2)(e => (e._1, e._3)).toList

    val staged =
      (grouped.traverse {
        case (i, dq) =>
          val (ds, qs) = dq.unzip
          for {
            pnext <- i.runRootValues(qs)
            next  <- completeAll(pnext._2, mapping)
          } yield (pnext._1 ++ next._1, ds.zip(next._2))
      }).map(Monoid.combineAll(_))

    staged.map {
      case (errors1, assoc) =>
        val subst = {
          val m = new java.util.IdentityHashMap[DeferredJson, Json]
          bad.foreach(dj => m.put(dj, Json.Null))
          assoc.foreach { case (d, j) => m.put(d, j) }
          m.asScala
        }
        val values = pjs.map(pj => scatterResults(pj, subst))
        (errors0 ++ errors1, values)
    }
  }

  def mkResponse(data: Option[Json], errors: List[Json]): Json = {
    val dataField = data.map { value => ("data", value) }.toList
    val errorField = if (errors.isEmpty) Nil else List(("errors", Json.fromValues(errors)))
    Json.fromFields(errorField ++ dataField)
  }

  def mkResponse(result: Result[Json]): Json =
    mkResponse(result.right, result.left.map(_.toList).getOrElse(Nil))

  def mkError(message: String, locations: List[(Int, Int)] = Nil, path: List[String] = Nil): Json = {
    val locationsField =
      if (locations.isEmpty) Nil
      else
        List((
          "locations",
          Json.fromValues(locations.map { case (line, col) => json""" { "line": $line, "col": $col } """ })
        ))
    val pathField =
      if (path.isEmpty) Nil
      else List(("path", Json.fromValues(path.map(Json.fromString))))

    Json.fromFields(("message", Json.fromString(message)) :: locationsField ++ pathField)
  }

  def mkErrorResult[T](message: String, locations: List[(Int, Int)] = Nil, path: List[String] = Nil): Result[T] =
    Ior.leftNec(mkError(message, locations, path))
}

trait ComponentMapping[F[_]] {
  val objectMappings: List[ObjectMapping]

  def isNoMapping = false

  def interpreterForType(tpe: Type): Option[QueryInterpreter[F]] =
    objectMappings.find(_.tpe =:= tpe).map(_.interpreter)

  def subobject(tpe: Type, fieldName: String): Option[Subobject] =
    objectMappings.find(_.tpe =:= tpe) match {
      case Some(om) =>
        om.fieldMappings.find(_._1 == fieldName) match {
          case Some((_, so: Subobject)) => Some(so)
          case _ => None
        }
      case None => None
    }

  sealed trait FieldMapping

  case class ObjectMapping(
    tpe: Type,
    interpreter: QueryInterpreter[F],
    fieldMappings: List[(String, FieldMapping)]
  )

  val defaultJoin: (Cursor, Query) => Result[Query] = (_, subquery: Query) => subquery.rightIor

  case class Subobject(
    submapping: ObjectMapping,
    subquery: (Cursor, Query) => Result[Query] = defaultJoin
  ) extends FieldMapping
}

object ComponentMapping {
  def NoMapping[F[_]]: ComponentMapping[F] =
    new ComponentMapping[F] {
      override def isNoMapping = true
      val objectMappings = Nil
    }
}

abstract class ComposedQueryInterpreter[F[_]: Monad](schema: Schema, mapping: ComponentMapping[F])
  extends QueryInterpreter[F](schema, mapping) {

  import QueryInterpreter.{ mkErrorResult, ProtoJson }

  import mapping._

  def elaborateSelect(tpe: Type, query: Select): Result[Query] =
    query.rightIor

  def runRootValue(query: Query): F[Result[ProtoJson]] = {
    query match {
      case Select(fieldName, _, _) =>
        subobject(schema.queryType, fieldName) match {
          case Some(Subobject(submapping, _)) => submapping.interpreter.runRootValue(query)
          case None => mkErrorResult("Bad query").pure[F]
        }
      case _ => mkErrorResult("Bad query").pure[F]
    }
  }
}
