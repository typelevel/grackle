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

/** GraphQL query Algebra */
sealed trait Query {
  /** Groups this query with its argument, Groups on either side are merged */
  def ~(query: Query): Query = (this, query) match {
    case (Group(hd), Group(tl)) => Group(hd ++ tl)
    case (hd, Group(tl)) => Group(hd :: tl)
    case (Group(hd), tl) => Group(hd :+ tl)
    case (hd, tl) => Group(List(hd, tl))
  }

  /** Yields a String representation of this query */
  def render: String
}

object Query {
  /** Select field `name` given arguments `args` and continue with `child` */
  case class Select(name: String, args: List[Binding], child: Query = Empty) extends Query {
    def render = {
      val rargs = if(args.isEmpty) "" else s"(${args.map(_.render).mkString(", ")})"
      val rchild = if(child == Empty) "" else s" { ${child.render} }"
      s"$name$rargs$rchild"
    }
  }

  /** A Group of sibling queries at the same level */
  case class Group(queries: List[Query]) extends Query {
    def render = queries.map(_.render).mkString(", ")
  }

  /** Picks out the unique element satisfying `pred` and continues with `child` */
  case class Unique(pred: Predicate, child: Query) extends Query {
    def render = s"<unique: $pred ${child.render}>"
  }

  /** Retains only elements satisfying `pred` and continuse with `child` */
  case class Filter(pred: Predicate, child: Query) extends Query {
    def render = s"<filter: $pred ${child.render}>"
  }

  /** Identifies a component boundary.
   *  `join` is applied to the current cursor and `child` yielding a continuation query which will be
   *  evaluated by the interpreter identified by `componentId`.
   */
  case class Component(componentId: String, join: (Cursor, Query) => Result[Query], child: Query) extends Query {
    def render = s"<component: $componentId ${child.render}>"
  }

  /** A deferred query.
   *  `join` is applied to the current cursor and `child` yielding a continuation query which will be
   *  evaluated by the current interpreter in its next stage.
   */
  case class Defer(join: (Cursor, Query) => Result[Query], child: Query) extends Query {
    def render = s"<defer: ${child.render}>"
  }

  /**
   * Wraps the result of `child` as a field named `name` of an enclosing object.
   */
  case class Wrap(name: String, child: Query) extends Query {
    def render = {
      val rchild = if(child == Empty) "" else s" { ${child.render} }"
      s"$name$rchild"
    }
  }

  /** The terminal query */
  case object Empty extends Query {
    def render = ""
  }

  /** InputValue binding */
  sealed trait Binding {
    def name: String
    type T
    val value: T

    def render: String
  }

  object Binding {
    import ScalarType._
    import QueryInterpreter.{ mkError, mkErrorResult }

    /** Given an actual argument `arg` and schema InputValue `info` yield an
     *  executable argument binding.
     *
     *  `Int`/`String`s will be resolved to `ID` if required by the schema.
     *  Untyped enum labels will be checked and mapped to suitable EnumValues.
     */
    def forArg(arg: Binding, info: InputValue): Result[Binding] =
      (info.tpe.asLeaf, arg) match {
        case (IntType, i: IntBinding) => i.rightIor
        case (FloatType, f: FloatBinding) => f.rightIor
        case (StringType, s: StringBinding) => s.rightIor
        case (BooleanType, b: BooleanBinding) => b.rightIor
        case (IDType, i: IntBinding) => IDBinding(i.name, i.value.toString).rightIor
        case (IDType, s: StringBinding) => IDBinding(s.name, s.value).rightIor
        case (e: EnumType, UntypedEnumBinding(name, value)) =>
          e.value(value).toRightIor(
            NonEmptyChain.one(mkError(s"Expected ${e.name} for argument '$name', found '$value'"))
          ).map { ev => EnumBinding(name, ev) }
        case (tpe, arg) =>
          mkErrorResult(s"Expected $tpe for argument '${arg.name}', found '${arg.value}'")
      }

    /** Given a schema InputValue `iv`, yield the default executable argument binding,
     *  if any.
     */
    def defaultForInputValue(iv: InputValue): Result[Binding] =
      (iv.tpe.asLeaf, iv.defaultValue) match {
        case (_, None) => NoBinding(iv.name, iv).rightIor
        case (IntType, Some(i: Int)) => IntBinding(iv.name, i).rightIor
        case (FloatType, Some(d: Double)) => FloatBinding(iv.name, d).rightIor
        case (StringType, Some(s: String)) => StringBinding(iv.name, s).rightIor
        case (BooleanType, Some(b: Boolean)) => BooleanBinding(iv.name, b).rightIor
        case (IDType, Some(i: Int)) => IDBinding(iv.name, i.toString).rightIor
        case (IDType, Some(s: String)) => IDBinding(iv.name, s).rightIor
        case (_: EnumType, Some(e: EnumValue)) => EnumBinding(iv.name, e).rightIor
        case _ => mkErrorResult(s"No argument and no default for $iv")
      }

    case class IntBinding(name: String, value: Int) extends Binding {
      type T = Int
      def render = s"""$name: $value"""
    }

    case class FloatBinding(name: String, value: Double) extends Binding {
      type T = Double
      def render = s"""$name: $value"""
    }

    case class StringBinding(name: String, value: String) extends Binding {
      type T = String
      def render = s"""$name: "$value""""
    }

    case class BooleanBinding(name: String, value: Boolean) extends Binding {
      type T = Boolean
      def render = s"""$name: $value"""
    }

    case class IDBinding(name: String, value: String) extends Binding {
      type T = String
      def render = s"""$name: "$value""""
    }

    case class UntypedEnumBinding(name: String, value: String) extends Binding {
      type T = String
      def render = s"""$name: $value"""
    }

    case class EnumBinding(name: String, value: EnumValue) extends Binding {
      type T = EnumValue
      def render = s"""$name: ${value.name}"""
    }

    case class NoBinding(name: String, value: InputValue) extends Binding {
      type T = InputValue
      def render = s"""$name: <undefined>"""
    }

    def toMap(bindings: List[Binding]): Map[String, Any] =
      bindings.map(b => (b.name, b.value)).toMap
  }
}

trait Predicate extends Product with (Cursor => Boolean) {
  def path: List[String]

  def prunePath(rootTpe: Type): (Type, String) = rootTpe.prunePath(path)

  def isField: Boolean

  override def toString = ScalaRunTime._toString(this)
}

trait FieldPredicate extends Predicate {
  def isField = true
}

trait AttributePredicate extends Predicate {
  def isField = false
}

object Predicate {
  object ScalarFocus {
    def unapply(c: Cursor): Option[Any] =
      if (c.isLeaf) Some(c.focus)
      else None
  }

  case class FieldEquals[T](fieldName: String, value: T) extends FieldPredicate {
    def path = List(fieldName)
    def apply(c: Cursor): Boolean =
      c.field(fieldName, Map.empty[String, Any]) match {
        case Ior.Right(ScalarFocus(focus)) => focus == value
        case _ => false
      }
  }

  case class FieldMatches(fieldName: String, r: Regex) extends FieldPredicate {
    def path = List(fieldName)
    def apply(c: Cursor): Boolean =
      c.field(fieldName, Map.empty[String, Any]) match {
        case Ior.Right(ScalarFocus(focus: String)) => r.matches(focus)
        case _ => false
      }
  }

  case class FieldContains[T](val path: List[String], value: T) extends FieldPredicate {
    def apply(c: Cursor): Boolean =
      c.listPath(path) match {
        case Ior.Right(cs) => cs.exists(_.focus == value)
        case _ => false
      }
  }

  case class AttrEquals[T](attrName: String, value: T) extends AttributePredicate {
    def path = List(attrName)
    def apply(c: Cursor): Boolean =
      c.attribute(attrName) match {
        case Ior.Right(`value`) => true
        case _ => false
      }
  }

  case class AttrMatches(attrName: String, r: Regex) extends AttributePredicate {
    def path = List(attrName)
    def apply(c: Cursor): Boolean =
      c.attribute(attrName) match {
        case Ior.Right(value: String) => r.matches(value)
        case _ => false
      }
  }

  case class AttrContains[T](val path: List[String], value: T) extends AttributePredicate {
    def apply(c: Cursor): Boolean =
      c.attrListPath(path) match {
        case Ior.Right(attrs) => attrs.exists(_ == value)
        case _ => false
      }
  }
}

abstract class QueryInterpreter[F[_]](implicit val F: Monad[F]) {

  def run(query: Query, rootTpe: Type): F[Json] =
    runRoot(query, rootTpe).map(QueryInterpreter.mkResponse)

  def complete(pj: ProtoJson): F[Result[Json]] =
    QueryInterpreter.complete[F](pj, Map.empty)

  def runRoot(query: Query, rootTpe: Type): F[Result[Json]] = {
    (for {
      pvalue <- IorT(runRootValue(query, rootTpe))
      value  <- IorT(complete(pvalue))
    } yield value).value
  }

  def runRootValue(query: Query, rootTpe: Type): F[Result[ProtoJson]]

  def runRootValues(queries: List[(Query, Type)]): F[(Chain[Json], List[ProtoJson])] =
    queries.traverse((runRootValue _).tupled).map { rs =>
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
        mkErrorResult(s"failed: { ${query.render} } $tpe")
    }
  }

  def runValue(query: Query, tpe: Type, cursor: Cursor): Result[ProtoJson] = {
    (query, tpe.dealias) match {
      case (Wrap(fieldName, child), _) =>
        for {
          pvalue <- runValue(child, tpe, cursor)
        } yield ProtoJson.fromFields(List((fieldName, pvalue)))

      case (Component(cid, join, child), _) =>
        for {
          cont <- join(cursor, child)
        } yield ProtoJson.component(cid, cont, tpe)

      case (Defer(join, child), _) =>
        for {
          cont <- join(cursor, child)
        } yield ProtoJson.staged(this, cont, tpe)

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
    private[QueryInterpreter] sealed trait DeferredJson
    private[QueryInterpreter] case class ComponentJson(componentId: String, query: Query, rootTpe: Type) extends DeferredJson
    private[QueryInterpreter] case class StagedJson[F[_]](interpreter: QueryInterpreter[F], query: Query, rootTpe: Type) extends DeferredJson
    private[QueryInterpreter] case class ProtoObject(fields: List[(String, ProtoJson)])
    private[QueryInterpreter] case class ProtoArray(elems: List[ProtoJson])

    def component(componentId: String, query: Query, rootTpe: Type): ProtoJson =
      wrap(ComponentJson(componentId, query, rootTpe))

    def staged[F[_]](interpreter: QueryInterpreter[F], query: Query, rootTpe: Type): ProtoJson =
      wrap(StagedJson(interpreter, query, rootTpe))

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

  def complete[F[_]: Monad](pj: ProtoJson, mapping: Map[String, QueryInterpreter[F]]): F[Result[Json]] =
    completeAll(List(pj), mapping).map {
      case (errors, List(value)) =>
        NonEmptyChain.fromChain(errors) match {
          case Some(errors) => Ior.Both(errors, value)
          case None => value.rightIor
        }
    }

  def completeAll[F[_]: Monad](pjs: List[ProtoJson], mapping: Map[String, QueryInterpreter[F]]): F[(Chain[Json], List[Json])] = {
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
      collected.foldLeft((List.empty[(DeferredJson, QueryInterpreter[F], (Query, Type))], List.empty[DeferredJson], Chain.empty[Json])) {
        case ((good, bad, errors), d@ComponentJson(cid, query, rootTpe)) =>
          mapping.get(cid) match {
            case Some(interpreter) =>
              ((d, interpreter, (query, rootTpe)) :: good, bad, errors)
            case None =>
              (good, d :: bad, mkError(s"No interpreter for query '${query.render}' which maps to component '$cid'") +: errors)
          }
        case ((good, bad, errors), d@StagedJson(interpreter, query, rootTpe)) =>
          ((d, interpreter.asInstanceOf[QueryInterpreter[F]], (query, rootTpe)) :: good, bad, errors)
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

  def mkInvalidResponse(result: Result[Query]): Json =
    mkResponse(None, result.left.map(_.toList).getOrElse(Nil))

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

class ComposedQueryInterpreter[F[_]: Monad](mapping: Map[String, QueryInterpreter[F]])
  extends QueryInterpreter[F] {

  override def complete(pj: ProtoJson): F[Result[Json]] =
    QueryInterpreter.complete(pj, mapping)

  def runRootValue(query: Query, rootTpe: Type): F[Result[ProtoJson]] = query match {
    case Wrap(_, Component(cid, _, child)) =>
      mapping.get(cid) match {
        case Some(interpreter) => interpreter.runRootValue(child, rootTpe)
        case None => mkErrorResult(s"No interpreter for query '${query.render}' which maps to component '$cid'").pure[F]
      }
    case _ => mkErrorResult(s"Bad root query '${query.render}' in ComposedQueryInterpreter").pure[F]
  }
}
