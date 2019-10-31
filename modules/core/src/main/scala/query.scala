// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

import cats.Monad
import cats.data.{ Chain, Ior, IorT, NonEmptyChain }
import cats.implicits._
import io.circe.Json
import io.circe.literal.JsonStringContext

sealed trait Query {
  import Query._

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

sealed trait ProtoJson {
  import ProtoJson._
  import QueryInterpreter.mkErrorResult

  def complete[F[_]: Monad](mapping: ComponentMapping[F]): F[Result[Json]] =
    this match {
      case PureJson(value) => value.rightIor.pure[F]

      case DeferredJson(cursor, tpe, fieldName, query) =>
        mapping.subobject(tpe, fieldName) match {
          case Some(mapping.Subobject(submapping, subquery)) =>
            (for {
              query  <- IorT(subquery(cursor, query).pure[F])
              pvalue <- IorT(submapping.interpreter.runRootValue(query))
              value  <- IorT(pvalue.complete(mapping))
            } yield value).value

          case _ => mkErrorResult(s"failed: ${tpe.shortString} $fieldName { ${query.render} }").pure[F]
        }

      case ProtoObject(fields) =>
        (fields.traverse { case (name, value) => value.complete(mapping).nested.map(v => (name, v)) }).map(Json.fromFields).value

      case ProtoArray(elems) =>
        elems.traverse(value => value.complete(mapping).nested).map(Json.fromValues).value
    }
}

object ProtoJson {
  import scala.collection.mutable

  import QueryInterpreter.mkError

  case class PureJson(value: Json) extends ProtoJson
  case class DeferredJson(cursor: Cursor, tpe: Type, fieldName: String, query: Query) extends ProtoJson
  case class ProtoObject(fields: List[(String, ProtoJson)]) extends ProtoJson
  case class ProtoArray(elems: List[ProtoJson]) extends ProtoJson

  def containsDeferred(pj: ProtoJson): Boolean =
    pj match {
      case _: PureJson => true
      case _ => false
    }

  def gatherDeferred(pj: ProtoJson): List[DeferredJson] = {
    @tailrec
    def loop(pending: Chain[ProtoJson], acc: List[DeferredJson]): List[DeferredJson] =
      pending.uncons match {
        case None => acc
        case Some((hd, tl)) => hd match {
          case _: PureJson         => loop(tl, acc)
          case d: DeferredJson     => loop(tl, d :: acc)
          case ProtoObject(fields) => loop(Chain.fromSeq(fields.map(_._2)) ++ tl, acc)
          case ProtoArray(elems)   => loop(Chain.fromSeq(elems) ++ tl, acc)
        }
      }

    loop(Chain.one(pj), Nil)
  }

  def scatterResults(pj: ProtoJson, results: mutable.Map[DeferredJson, ProtoJson]): ProtoJson = {
    def loop(pj: ProtoJson): ProtoJson =
      pj match {
        case p: PureJson       => p
        case d: DeferredJson   => results.getOrElse(d, d)
        case o@ProtoObject(fields) =>
          val values = fields.map(f => loop(f._2))
          if (values.corresponds(fields.iterator.map(_._2))(_ eq _)) o
          else ProtoObject(fields.iterator.map(_._1).zip(values).toList)
        case a@ProtoArray(elems) =>
          val elems0 = elems.map(loop)
          if (elems0.corresponds(elems)(_ eq _)) a
          else ProtoArray(elems0)
      }

    loop(pj)
  }

  def mkSubstMap(good: List[(DeferredJson, Result[ProtoJson])], bad: List[DeferredJson]): mutable.Map[DeferredJson, ProtoJson] = {
    val m = new java.util.IdentityHashMap[DeferredJson, ProtoJson]
    bad.foreach(dj => m.put(dj, PureJson(Json.Null)))
    good.foreach {
      case (dj, rv) => rv.right match {
        case Some(v) => m.put(dj, v)
        case None => m.put(dj, PureJson(Json.Null))
      }
    }
    m.asScala
  }

  def batch[F[_]: Monad](pj: ProtoJson, mapping: ComponentMapping[F]): F[Result[ProtoJson]] = {
    val collected = gatherDeferred(pj)

    val (good, bad, errors) =
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

    val grouped: F[List[(DeferredJson, Result[ProtoJson])]] =
      good.groupMap(_._2)(e => (e._1, e._3)).toList.flatTraverse { case (i, dq) =>
        val (ds, qs) = dq.unzip
        i.runRootValues(qs).map(ds.zip(_))
      }

    grouped.map { substs =>
      val value = scatterResults(pj, mkSubstMap(substs, bad))
      NonEmptyChain.fromChain(errors) match {
        case Some(errors) => Ior.Both(errors, value)
        case None => value.rightIor
      }
    }
  }

  def deferred(cursor: Cursor, tpe: Type, fieldName: String, query: Query): ProtoJson =
    DeferredJson(cursor, tpe, fieldName, query)

  def fromJson(value: Json): ProtoJson = PureJson(value)

  def fromFields(fields: List[(String, ProtoJson)]): ProtoJson =
    if(fields.forall(_._2.isInstanceOf[PureJson]))
      PureJson(Json.fromFields(fields.map { case (name, c) => (name, c.asInstanceOf[PureJson].value) }))
    else
      ProtoObject(fields)

  def fromValues(elems: List[ProtoJson]): ProtoJson =
    if(elems.forall(_.isInstanceOf[PureJson]))
      PureJson(Json.fromValues(elems.map(_.asInstanceOf[PureJson].value)))
    else
      ProtoArray(elems)
}

abstract class QueryInterpreter[F[_]](implicit val F: Monad[F]) {
  import Query._
  import QueryInterpreter.mkErrorResult
  import ComponentMapping.NoMapping

  val schema: Schema

  def run(query: Query, mapping: ComponentMapping[F] = NoMapping): F[Json] =
    runRoot(query, mapping).map(QueryInterpreter.mkResponse)

  def runRoot(query: Query, mapping: ComponentMapping[F] = NoMapping): F[Result[Json]] =
    query match {
      case Select(fieldName, _, _) =>
        (for {
          pvalue <- IorT(runRootValue(query))
          value  <- IorT(pvalue.complete(mapping))
        } yield Json.obj((fieldName, value))).value

      case _ =>
        mkErrorResult(s"Bad query: $query").pure[F]
    }

  def runRootValue(query: Query): F[Result[ProtoJson]]

  def runRootValues(query: List[Query]): F[List[Result[ProtoJson]]] =
    query.traverse(runRootValue)

  def runFields(query: Query, tpe: Type, cursor: Cursor): F[Result[List[(String, ProtoJson)]]] = {
    (query, tpe) match {
      case (sel@Select(fieldName, _, _), NullableType(tpe)) =>
        cursor.asNullable.sequence.map { rc =>
          (for {
            c      <- IorT(rc.pure[F])
            fields <- IorT(runFields(sel, tpe, c))
          } yield fields).value
        }.getOrElse(List((fieldName, ProtoJson.fromJson(Json.Null))).rightIor.pure[F])

      case (Select(fieldName, bindings, child), tpe) =>
        if (!cursor.hasField(fieldName))
          List((fieldName, ProtoJson.deferred(cursor, tpe, fieldName, child))).rightIor.pure[F]
        else
          (for {
            c     <- IorT(cursor.field(fieldName, Binding.toMap(bindings)).pure[F])
            value <- IorT(runValue(child, tpe.field(fieldName), c))
          } yield List((fieldName, value))).value

      case (Group(siblings), _) =>
        siblings.flatTraverse(query => runFields(query, tpe, cursor).nested).value

      case _ =>
        mkErrorResult(s"failed: { ${query.render} } ${tpe.shortString}").pure[F]
    }
  }

  def runValue(query: Query, tpe: Type, cursor: Cursor): F[Result[ProtoJson]] = {
    tpe match {
      case NullableType(tpe) =>
        cursor.asNullable.sequence.map { rc =>
          (for {
            c     <- IorT(rc.pure[F])
            value <- IorT(runValue(query, tpe, c))
          } yield value).value
        }.getOrElse(ProtoJson.fromJson(Json.Null).rightIor.pure[F])

      case ListType(tpe) =>
        cursor.asList.flatTraverse(lc =>
          lc.traverse(c => runValue(query, tpe, c).nested).map(ProtoJson.fromValues).value
        )

      case TypeRef(schema, tpnme) =>
        schema.types.find(_.name == tpnme)
          .map(tpe => runValue(query, tpe, cursor))
          .getOrElse(mkErrorResult(s"Unknown type '$tpnme'").pure[F])

      case (_: ScalarType) | (_: EnumType) => cursor.asLeaf.map(ProtoJson.fromJson).pure[F]

      case (_: ObjectType) | (_: InterfaceType) =>
        runFields(query, tpe, cursor).nested.map(ProtoJson.fromFields).value

      case _ =>
        mkErrorResult(s"Unsupported type $tpe").pure[F]
    }
  }
}

object QueryInterpreter {
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
      val objectMappings = Nil
    }
}

trait ComposedQueryInterpreter[F[_]] extends QueryInterpreter[F] with ComponentMapping[F] {
  import Query._
  import QueryInterpreter.mkErrorResult

  def run(query: Query): F[Json] = run(query, this)

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
