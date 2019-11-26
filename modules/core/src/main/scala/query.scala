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

import Query._
import QueryInterpreter.{ mkErrorResult, ProtoJson }

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
    def render = s"<unique: $pred ${child.render}>"
  }
  case class Filter(pred: Predicate, child: Query) extends Query {
    def render = s"<filter: $pred ${child.render}>"
  }
  case class Component(schema: SchemaComponent, join: (Cursor, Query) => Result[Query], child: Query) extends Query {
    def render = s"<component: ${schema.getClass.getSimpleName} ${child.render}>"
  }
  case class Defer(join: (Cursor, Query) => Result[Query], child: Query) extends Query {
    def render = s"<defer: ${child.render}>"
  }
  case class Wrap(name: String, child: Query) extends Query {
    def render = {
      val rchild = if(child == Empty) "" else s" { ${child.render} }"
      s"$name$rchild"
    }
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
  def path: List[String]

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
    def path = List(fieldName)
    def apply(c: Cursor): Boolean =
      c.field(fieldName, Map.empty[String, Any]) match {
        case Ior.Right(StringScalarFocus(`value`)) => true
        case _ => false
      }
  }

  case class FieldMatches(fieldName: String, r: Regex) extends Predicate {
    def path = List(fieldName)
    def apply(c: Cursor): Boolean =
      c.field(fieldName, Map.empty[String, Any]) match {
        case Ior.Right(StringScalarFocus(value)) => r.matches(value)
        case _ => false
      }
  }

  case class FieldContains(val path: List[String], value: String) extends Predicate {
    def apply(c: Cursor): Boolean =
      c.listPath(path) match {
        case Ior.Right(cs) =>
          cs.exists {
            case StringScalarFocus(`value`) => true
            case _ => false
          }
        case _ => false
      }
  }

  case class AttrEquals(attrName: String, value: String) extends Predicate {
    def path = List(attrName)
    def apply(c: Cursor): Boolean =
      c.attribute(attrName) match {
        case Ior.Right(`value`) => true
        case _ => false
      }
  }

  case class AttrMatches(attrName: String, r: Regex) extends Predicate {
    def path = List(attrName)
    def apply(c: Cursor): Boolean =
      c.attribute(attrName) match {
        case Ior.Right(value: String) => r.matches(value)
        case _ => false
      }
  }

  case class AttrContains(val path: List[String], value: String) extends Predicate {
    def apply(c: Cursor): Boolean =
      c.attrListPath(path) match {
        case Ior.Right(attrs) => attrs.exists(_ == value)
        case _ => false
      }
  }
}

abstract class QueryInterpreter[F[_]](val schema: Schema)(implicit val F: Monad[F]) {

  def run(query: Query): F[Json] =
    runRoot(query).map(QueryInterpreter.mkResponse)

  def complete(pj: ProtoJson): F[Result[Json]] =
    QueryInterpreter.complete(pj, Map(schema -> this))

  def runRoot(query: Query): F[Result[Json]] = {
    (for {
      pvalue <- IorT(runRootValue(query))
      value  <- IorT(complete(pvalue))
    } yield value).value
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
        for {
          c     <- cursor.field(fieldName, Binding.toMap(bindings))
          value <- runValue(child, tpe.field(fieldName), c)
        } yield List((fieldName, value))

      case (Wrap(fieldName, child), tpe) =>
        for {
          value <- runValue(child, tpe, cursor)
        } yield List((fieldName, value))

      case (Group(siblings), _) =>
        siblings.flatTraverse(query => runFields(query, tpe, cursor))

      case _ =>
        mkErrorResult(s"failed: { ${query.render} } ${tpe.shortString}")
    }
  }

  def runValue(query: Query, tpe: Type, cursor: Cursor): Result[ProtoJson] = {
    (query, tpe.dealias) match {
      case (Wrap(fieldName, child), _) =>
        for {
          pvalue <- runValue(child, tpe, cursor)
        } yield ProtoJson.fromFields(List((fieldName, pvalue)))

      case (Component(schema, join, child), _) =>
        for {
          cont <- join(cursor, child)
        } yield ProtoJson.deferred(schema, cont)

      case (Defer(join, child), _) =>
        for {
          cont <- join(cursor, child)
        } yield ProtoJson.deferred(schema, cont)

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

      case (_, (_: ScalarType) | (_: EnumType)) =>
        cursor.asLeaf.map(ProtoJson.fromJson)

      case (_, (_: ObjectType) | (_: InterfaceType)) =>
        runFields(query, tpe, cursor).map(ProtoJson.fromFields)

      case _ =>
        mkErrorResult(s"Stuck at type $tpe for ${query.render}")
    }
  }
}

object QueryInterpreter {
  type ProtoJson <: AnyRef

  object ProtoJson {
    private[QueryInterpreter] case class DeferredJson(schema: SchemaComponent, query: Query)
    private[QueryInterpreter] case class ProtoObject(fields: List[(String, ProtoJson)])
    private[QueryInterpreter] case class ProtoArray(elems: List[ProtoJson])

    def deferred(schema: SchemaComponent, query: Query): ProtoJson =
      wrap(DeferredJson(schema, query))

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

    def isDeferred(p: ProtoJson): Boolean =
      p.isInstanceOf[DeferredJson]

    private def wrap(j: AnyRef): ProtoJson = j.asInstanceOf[ProtoJson]
  }

  import ProtoJson._

  def complete[F[_]: Monad](pj: ProtoJson, mapping: Map[SchemaComponent, QueryInterpreter[F]]): F[Result[Json]] =
    completeAll(List(pj), mapping).map {
      case (errors, List(value)) =>
        NonEmptyChain.fromChain(errors) match {
          case Some(errors) => Ior.Both(errors, value)
          case None => value.rightIor
        }
    }

  def completeAll[F[_]: Monad](pjs: List[ProtoJson], mapping: Map[SchemaComponent, QueryInterpreter[F]]): F[(Chain[Json], List[Json])] = {
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
            val newFields: List[(String, Json)] =
              fields.flatMap { case (label, pvalue) =>
                val value = loop(pvalue)
                if (isDeferred(pvalue) && value.isObject) value.asObject.get.toList
                else List((label, value))
              }
            Json.fromFields(newFields)

          case ProtoArray(elems) =>
            val elems0 = elems.map(loop)
            Json.fromValues(elems0)
        }

      loop(pj)
    }

    val collected = pjs.flatMap(gatherDeferred)

    val (good, bad, errors0) =
      collected.foldLeft((List.empty[(DeferredJson, QueryInterpreter[F], Query)], List.empty[DeferredJson], Chain.empty[Json])) {
        case ((good, bad, errors), d@DeferredJson(schema, query)) =>
          mapping.get(schema) match {
            case Some(interpreter) =>
              ((d, interpreter, query) :: good, bad, errors)
            case None =>
              (good, d :: bad, mkError(s"Bad query: ${schema.getClass.getName} ${query.render}") +: errors)
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

class ComposedQueryInterpreter[F[_]: Monad](schema: Schema, mapping: Map[SchemaComponent, QueryInterpreter[F]])
  extends QueryInterpreter[F](schema) {

  override def complete(pj: ProtoJson): F[Result[Json]] =
    QueryInterpreter.complete(pj, mapping)

  def runRootValue(query: Query): F[Result[ProtoJson]] = query match {
    case Wrap(_, Component(schema, _, child)) =>
      mapping.get(schema) match {
        case Some(interpreter) => interpreter.runRootValue(child)
        case None => mkErrorResult("Bad query").pure[F]
      }
    case _ => mkErrorResult("Bad query").pure[F]
  }
}
