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

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._

import cats.{Monad, Monoid}
import cats.data.{ Chain, NonEmptyChain }
import cats.implicits._
import fs2.Stream
import io.circe.Json

import syntax._
import Cursor.ListTransformCursor
import Query._
import QueryInterpreter.ProtoJson
import ProtoJson._

class QueryInterpreter[F[_]](mapping: Mapping[F]) {
  import mapping.{M, RootCursor, RootEffect, RootStream}

  /** Interpret `query` with expected type `rootTpe`.
   *
   *  The query is fully interpreted, including deferred or staged
   *  components.
   *
   *  GraphQL errors are accumulated in the result.
   */
  def run(query: Query, rootTpe: Type, env: Env): Stream[F, Result[Json]] = {
    val rootCursor = RootCursor(Context(rootTpe), None, env)
    val mergedResults =
      if(mapping.schema.subscriptionType.exists(_ =:= rootTpe))
        runSubscription(query, rootTpe, rootCursor)
      else
        Stream.eval(runOneShot(query, rootTpe, rootCursor))

    (for {
      pvalue <- ResultT(mergedResults)
      value  <- ResultT(Stream.eval(QueryInterpreter.complete[F](pvalue)))
    } yield value).value
  }

  /**
   *  Run a subscription query yielding a stream of results.
   */
  def runSubscription(query: Query, rootTpe: Type, rootCursor: Cursor): Stream[F, Result[ProtoJson]] =
    ungroup(query) match {
      case Nil => Result(ProtoJson.fromJson(Json.Null)).pure[Stream[F, *]]
      case List(root) =>
        (for {
          rootName                      <- Query.rootName(root)
          RootStream(fieldName, effect) <- mapping.rootStream(Context(rootTpe), rootName._1).orElse(mapping.rootEffect(Context(rootTpe), rootName._1).map(_.toRootStream))
        } yield
            effect(root, rootTpe / fieldName, rootCursor.fullEnv.addFromQuery(root)).map(_.flatMap { // TODO Rework in terms of cursor
              case (q, c) => runValue(q, rootTpe, c)
            })
        ).getOrElse(Result.internalError("EffectMapping required for subscriptions").pure[Stream[F, *]])

      case _ =>
        Result.internalError("Only one root selection permitted for subscriptions").pure[Stream[F, *]]
    }

  /**
   *  Run a non-subscription query yielding a single result.
   */
  def runOneShot(query: Query, rootTpe: Type, rootCursor: Cursor): F[Result[ProtoJson]] = {
    case class PureQuery(query: Query)
    case class EffectfulQuery(query: Query, rootEffect: RootEffect)

    val rootContext = Context(rootTpe)
    val ungrouped = ungroup(query)
    val hasRootStream =
      ungrouped.exists { root =>
        Query.rootName(root).flatMap(rootName => mapping.rootStream(rootContext, rootName._1)).isDefined
      }

    if(hasRootStream)
      Result.internalError("RootStream only permitted in subscriptions").pure[F].widen
    else {
      val (effectfulQueries, pureQueries) = ungrouped.partitionMap { query =>
        (for {
          rootName <- Query.rootName(query)
          re       <- mapping.rootEffect(rootContext, rootName._1)
        } yield Left(EffectfulQuery(query, re))).getOrElse(Right(PureQuery(query)))
      }

      val pureResults: F[List[Result[ProtoJson]]] =
        if(pureQueries.isEmpty) Nil.pure[F].widen
        else {
          val (introQueries, nonIntroQueries) = pureQueries.partitionMap {
            case PureQuery(i: Introspect) => Left(i)
            case PureQuery(other) => Right(other)
          }

          val introResults: List[Result[ProtoJson]] =
            introQueries.flatMap {
              case Introspect(schema, query) =>
                val interp = Introspection.interpreter(schema)
                interp.runRootValue(query, Introspection.schema.queryType, rootCursor) match {
                  case Right(res) => List(res)
                  case Left(err) => List(Result.internalError(err))
                }
            }

          val nonIntroResults: F[List[Result[ProtoJson]]] =
            nonIntroQueries match {
              case Nil => Nil.pure[F].widen
              case List(q) => runRootValue(q, rootTpe, rootCursor).map(List(_))
              case qs => runRootValue(Group(qs), rootTpe, rootCursor).map(List(_))
            }

          nonIntroResults.map(_ ++ introResults)
        }

      val effectfulResults: F[List[Result[ProtoJson]]] =
        if(effectfulQueries.isEmpty) Nil.pure[F].widen
        else {
          effectfulQueries.traverse {
            case EffectfulQuery(query, RootEffect(fieldName, effect)) =>
              effect(query, rootTpe / fieldName, rootCursor.fullEnv.addFromQuery(query)).map(_.flatMap { // TODO Rework in terms of cursor
                case (q, c) => runValue(q, rootTpe, c)
              })
          }
        }

      for {
        pr <- pureResults
        er <- effectfulResults
      } yield
        ((pr ++ er) match {
          case Nil => Result(ProtoJson.fromJson(Json.Null))
          case List(r) => r
          case hd :: tl =>
            tl.foldLeft(hd) {
              case (acc, elem) => acc |+| elem
            }
        }) match {
          case Result.Failure(errs) => Result.Warning(errs, ProtoJson.fromJson(Json.Null))
          case other => other
        }
    }
  }

  /** Interpret `query` with expected type `rootTpe`.
   *
   *  At most one stage will be run and the result may contain deferred
   *  components.
   *
   *  Errors are accumulated on the `Left` of the result.
   */
  def runRootValue(query: Query, rootTpe: Type, parentCursor: Cursor): F[Result[ProtoJson]] =
    (for {
      qc       <- ResultT(mapping.defaultRootCursor(query, rootTpe, Some(parentCursor)))
      value    <- ResultT(runValue(qc._1, rootTpe, qc._2).pure[F])
    } yield value).value

  def cursorCompatible(tpe: Type, cursorTpe: Type): Boolean = {
    def strip(tpe: Type): Type =
      tpe.dealias match {
        case NullableType(tpe) => strip(tpe)
        case ListType(tpe) => strip(tpe)
        case _ => tpe
      }

    (strip(tpe).isLeaf && strip(cursorTpe).isLeaf) ||
    (strip(tpe) nominal_=:= strip(cursorTpe))
  }

  /**
   * Interpret `query` against `cursor`, yielding a collection of fields.
   *
   * If the query is valid, the field subqueries will all be valid fields
   * of the enclosing type `tpe` and the resulting fields may be used to
   * build a Json object of type `tpe`. If the query is invalid errors
   * will be returned on the left hand side of the result.
   */
  def runFields(query: Query, tpe: Type, cursor: Cursor): Result[List[(String, ProtoJson)]] =
    if (!cursorCompatible(tpe, cursor.tpe))
      Result.internalError(s"Mismatched query and cursor type in runFields: $tpe ${cursor.tpe}")
    else {
      query match {
        case TypeCase(default, narrows) =>
          val runDefault =
            default match {
              case Empty => Nil.success
              case _ => runFields(default, tpe, cursor)
            }
          val applicableNarrows = narrows.filter(n => cursor.narrowsTo(n.subtpe))
          val runApplicableNarrows = applicableNarrows.flatTraverse {
            case Narrow(tp1, child) =>
              for {
                c      <- cursor.narrow(tp1)
                fields <- runFields(child, tp1, c)
              } yield fields
          }

          for {
            default <- runDefault
            applicableNarrows <- runApplicableNarrows
          } yield mergeFields(default ::: applicableNarrows).toList

        case Group(siblings) =>
          siblings.flatTraverse(query => runFields(query, tpe, cursor))

        case Introspect(schema, s@Select("__typename", _, Empty)) if tpe.isNamed =>
          (tpe.dealias match {
            case o: ObjectType => Some(o.name)
            case i: InterfaceType =>
              (schema.types.collectFirst {
                case o: ObjectType if o <:< i && cursor.narrowsTo(schema.uncheckedRef(o)) => o.name
              })
            case u: UnionType =>
              (u.members.map(_.dealias).collectFirst {
                case nt: NamedType if cursor.narrowsTo(schema.uncheckedRef(nt)) => nt.name
              })
            case _ => None
          }) match {
            case Some(name) =>
              List((s.resultName, ProtoJson.fromJson(Json.fromString(name)))).success
            case None =>
              Result.failure(s"'__typename' cannot be applied to non-selectable type '$tpe'")
          }

        case sel: Select if tpe.isNullable =>
          cursor.asNullable.sequence.map { rc =>
            for {
              c      <- rc
              fields <- runFields(sel, tpe, c)
            } yield fields
          }.getOrElse(List((sel.resultName, ProtoJson.fromJson(Json.Null))).success)

        case sel@Select(_, _, Count(Select(countName, _, _))) =>
          def size(c: Cursor): Result[Int] =
            if (c.isList) c.asList(Iterator).map(_.size)
            else 1.success

          for {
            c0     <- cursor.field(countName, None)
            count  <- if (c0.isNullable) c0.asNullable.flatMap(_.map(size).getOrElse(0.success))
                      else size(c0)
          } yield List((sel.resultName, ProtoJson.fromJson(Json.fromInt(count))))

        case sel@Select(_, _, Effect(handler, cont)) =>
          for {
            value <- ProtoJson.effect(mapping, handler.asInstanceOf[EffectHandler[F]], cont, cursor).success
          } yield List((sel.resultName, value))

        case sel@Select(fieldName, resultName, child) =>
          val fieldTpe = tpe.field(fieldName).getOrElse(ScalarType.AttributeType)
          for {
            c        <- cursor.field(fieldName, resultName)
            value    <- runValue(child, fieldTpe, c)
          } yield List((sel.resultName, value))

        case c@Component(_, _, cont) =>
          for {
            componentName <- resultName(cont).toResultOrError("Join continuation has unexpected shape")
            value <- runValue(c, tpe, cursor)
          } yield List((componentName, ProtoJson.select(value, componentName)))

        case Environment(childEnv: Env, child: Query) =>
          runFields(child, tpe, cursor.withEnv(childEnv))

        case TransformCursor(f, child) =>
          for {
            ct     <- f(cursor)
            fields <- runFields(child, tpe, ct)
          } yield fields

        case _ =>
          Result.internalError(s"runFields failed: { ${query.render} } $tpe")
      }
    }

  def runList(query: Query, tpe: Type, parent: Cursor, unique: Boolean, nullable: Boolean): Result[ProtoJson] = {
    val (query0, f) =
      query match {
        case TransformCursor(f, child) => (child, Some(f))
        case _ => (query, None)
      }

    def transformElems(cs: Iterator[Cursor]): Result[Iterator[Cursor]] =
      f match {
        case None => cs.success
        case Some(f) =>
          val cs0 = cs.toSeq
          val tc = ListTransformCursor(parent, cs0.size, cs0)
          f(tc).flatMap(_.asList(Iterator))
      }

    def applyOps(cursors: Iterator[Cursor]): Result[(Query, Iterator[Cursor])] = {
      query0 match {
        case FilterOrderByOffsetLimit(pred, selections, offset, limit, child) =>
          val sorted =
            if(pred.isEmpty && selections.isEmpty) cursors
            else {
              val cs = cursors.toSeq
              val filtered =
                pred match {
                  case Some(p) =>
                    cs.filterA(p(_)) match {
                      case err@Result.InternalError(_) => return err
                      case fail@Result.Failure(_) => return fail
                      case Result.Success(cs) => cs
                      case Result.Warning(_, cs) => cs
                    }
                  case _ => cs
                }
              selections.map(OrderSelections(_).order(filtered)).getOrElse(filtered).iterator
            }
          val sliced = (offset, limit) match {
            case (None, None) => sorted
            case (Some(off), None) => sorted.drop(off)
            case (None, Some(lim)) => sorted.take(lim)
            case (Some(off), Some(lim)) => sorted.slice(off, off+lim)
          }
          transformElems(sliced).map(cs => (child, cs))
        case other =>
          transformElems(cursors).map(cs => (other, cs))
      }
    }

    def mkResult(child: Query, ic: Iterator[Cursor]): Result[ProtoJson] = {
      val builder = Vector.newBuilder[ProtoJson]
      var problems = Chain.empty[Problem]
      builder.sizeHint(ic.knownSize)
      while(ic.hasNext) {
        val c = ic.next()
        if (!cursorCompatible(tpe, c.tpe))
          return Result.internalError(s"Mismatched query and cursor type in runList: $tpe ${c.tpe}")

        runValue(child, tpe, c) match {
          case err@Result.InternalError(_) => return err
          case fail@Result.Failure(_) => return fail
          case Result.Success(v) => builder.addOne(v)
          case Result.Warning(ps, v) =>
            builder.addOne(v)
            problems = problems.concat(ps.toChain)
        }
      }

      def mkResult(j: ProtoJson): Result[ProtoJson] =
        NonEmptyChain.fromChain(problems).map(neps => Result.Warning(neps, j)).getOrElse(j.success)

      if (!unique) mkResult(ProtoJson.fromValues(builder.result()))
      else {
        val size = builder.knownSize
        if (size == 1) mkResult(builder.result()(0))
        else if (size == 0) {
          if(nullable) mkResult(ProtoJson.fromJson(Json.Null))
          else Result.internalError(s"No match")
        } else Result.internalError(s"Multiple matches")
      }
    }

    for {
      cursors     <- parent.asList(Iterator)
      ccs         <- applyOps(cursors)
      (child, cs) =  ccs
      res         <- mkResult(child, cs)
    } yield res
  }

  /**
   * Interpret `query` against `cursor` with expected type `tpe`.
   *
   * If the query is invalid errors will be returned on the left hand side
   * of the result.
   */
  def runValue(query: Query, tpe: Type, cursor: Cursor): Result[ProtoJson] = {
    if (!cursorCompatible(tpe, cursor.tpe))
      Result.internalError(s"Mismatched query and cursor type in runValue: $tpe ${cursor.tpe}")
    else {
      (query, tpe.dealias) match {
        case (Environment(childEnv: Env, child: Query), tpe) =>
          runValue(child, tpe, cursor.withEnv(childEnv))

        case (Component(_, _, _), ListType(tpe)) =>
          cursor.asList(Iterator) match {
            case Result.Success(ic) =>
              val builder = Vector.newBuilder[ProtoJson]
              builder.sizeHint(ic.knownSize)
              while(ic.hasNext) {
                val c = ic.next()
                runValue(query, tpe, c) match {
                  case Result.Success(v) => builder.addOne(v)
                  case notRight => return notRight
                }
              }
              ProtoJson.fromValues(builder.result()).success
            case Result.Warning(ps, _) => Result.Failure(ps)
            case fail@Result.Failure(_) => fail
            case err@Result.InternalError(_) => err
          }

        case (Component(mapping, join, child), _) =>
          join(child, cursor).flatMap {
            case Group(conts) =>
              for {
                childName <- resultName(child).toResultOrError("Join child has unexpected shape")
                elems     <- conts.traverse { case cont =>
                              for {
                                componentName <- resultName(cont).toResultOrError("Join continuation has unexpected shape")
                              } yield
                                ProtoJson.select(
                                  ProtoJson.component(mapping, cont, cursor),
                                  componentName
                                )
                              }
              } yield
                ProtoJson.fromDisjointFields(
                  List(childName -> ProtoJson.fromValues(elems))
                )

            case cont =>
              for {
                renamedCont <- alignResultName(child, cont).toResultOrError("Join continuation has unexpected shape")
              } yield ProtoJson.component(mapping, renamedCont, cursor)
          }

        case (Unique(child), _) =>
          cursor.preunique.flatMap(c =>
            runList(child, tpe.nonNull, c, true, tpe.isNullable)
          )

        case (_, ListType(tpe)) =>
          runList(query, tpe, cursor, false, false)

        case (TransformCursor(f, child), _) =>
          for {
            ct    <- f(cursor)
            value <- runValue(child, tpe, ct)
          } yield value

        case (_, NullableType(tpe)) =>
          cursor.asNullable.sequence.map { rc =>
            for {
              c     <- rc
              value <- runValue(query, tpe, c)
            } yield value
          }.getOrElse(ProtoJson.fromJson(Json.Null).success)

        case (_, (_: ScalarType) | (_: EnumType)) =>
          cursor.asLeaf.map(ProtoJson.fromJson)

        case (_, (_: ObjectType) | (_: InterfaceType) | (_: UnionType)) =>
          runFields(query, tpe, cursor).map(ProtoJson.fromDisjointFields)

        case _ =>
          Result.internalError(s"Stuck at type $tpe for ${query.render}")
      }
    }
  }
}

object QueryInterpreter {
  /**
   * Opaque type of partially constructed query results.
   *
   * Values may be fully expanded Json values, objects or arrays which not
   * yet fully evaluated subtrees, or subqueries which are deferred to the
   * next stage or another component of a composite interpreter.
   */
  type ProtoJson <: AnyRef

  object ProtoJson {
    private[QueryInterpreter] sealed trait DeferredJson
    // A result which depends on an effect and a continuation in the next stage of this or another interpreter.
    private[QueryInterpreter] case class EffectJson[F[_]](mapping: Mapping[F], handler: Option[EffectHandler[F]], query: Query, cursor: Cursor) extends DeferredJson
    // A partially constructed object which has at least one deferred subtree.
    private[QueryInterpreter] case class ProtoObject(fields: Seq[(String, ProtoJson)])
    // A partially constructed array which has at least one deferred element.
    private[QueryInterpreter] case class ProtoArray(elems: Seq[ProtoJson])
    // A result which will yield a selection from its child
    private[QueryInterpreter] case class ProtoSelect(elem: ProtoJson, fieldName: String)

    implicit val monoidInstance: Monoid[ProtoJson] =
      new Monoid[ProtoJson] {
        val empty: ProtoJson = fromJson(Json.Null)
        def combine(x: ProtoJson, y: ProtoJson): ProtoJson = ProtoJson.mergeProtoJson(List(x, y))
      }

    /**
     * Delegate `query` to the interpreter `interpreter`. When evaluated by
     * that interpreter the query will have expected type `rootTpe`.
     */
    def component[F[_]](mapping: Mapping[F], query: Query, cursor: Cursor): ProtoJson =
      wrap(EffectJson(mapping, None, query, cursor))

    def effect[F[_]](mapping: Mapping[F], handler: EffectHandler[F], query: Query, cursor: Cursor): ProtoJson =
      wrap(EffectJson(mapping, Some(handler), query, cursor))

    def fromJson(value: Json): ProtoJson = wrap(value)

    /**
     * Combine possibly partial fields to create a possibly partial object.
     *
     * If all fields are complete then they will be combined as a complete
     * Json object.
     *
     * Assumes that all fields are disjoint.
     */
    def fromDisjointFields(fields: Seq[(String, ProtoJson)]): ProtoJson =
      if(fields.forall(_._2.isInstanceOf[Json]))
        wrap(Json.fromFields(fields.asInstanceOf[Seq[(String, Json)]]))
      else
        wrap(ProtoObject(fields))

    /**
     * Combine possibly partial fields to create a possibly partial object.
     *
     * If all fields are complete then they will be combined as a complete
     * Json object.
     */
    def fromFields(fields: Seq[(String, ProtoJson)]): ProtoJson =
      fromDisjointFields(mergeFields(fields))

    /**
     * Combine possibly partial values to create a possibly partial array.
     *
     * If all values are complete then they will be combined as a complete
     * Json array.
     */
    def fromValues(elems: Seq[ProtoJson]): ProtoJson =
      if(elems.forall(_.isInstanceOf[Json]))
        wrap(Json.fromValues(elems.asInstanceOf[Seq[Json]]))
      else
        wrap(ProtoArray(elems))

    /**
     * Select a value from a possibly partial object.
     *
     * If the object is complete the selection will be a complete
     * Json value.
     */
    def select(elem: ProtoJson, fieldName: String): ProtoJson =
      elem match {
        case j: Json =>
          wrap(j.asObject.flatMap(_(fieldName)).getOrElse(Json.Null))
        case _ =>
          wrap(ProtoSelect(elem, fieldName))
      }

    /**
     * Test whether the argument contains any deferred subtrees
     *
     * Yields `true` if the argument contains any component or staged
     * subtrees, false otherwise.
     */
    def isDeferred(p: ProtoJson): Boolean =
      p.isInstanceOf[DeferredJson]

    /** Recursively merge a list of ProtoJson values. */
    def mergeProtoJson(elems: Seq[ProtoJson]): ProtoJson = {
      elems match {
        case Seq(elem) => elem
        case Seq(_: ProtoObject, _*) => mergeProtoObjects(elems)
        case Seq(j: Json, _*) if j.isObject => mergeProtoObjects(elems)
        case Seq(_: ProtoArray, _*) => mergeProtoArrays(elems)
        case Seq(j: Json, _*) if j.isArray => mergeProtoArrays(elems)
        case Seq(hd, _*) => hd
        case _ => wrap(Json.Null)
      }
    }

    /** Recursively merge a list of ProtoJson objects. */
    def mergeProtoObjects(objs: Seq[ProtoJson]): ProtoJson =
      objs match {
        case Seq(obj) => obj
        case Seq(_, _, _*) =>
          val fieldss = objs flatMap {
            case ProtoObject(fields) => fields
            case j: Json if j.isObject => j.asObject.get.toIterable.map { case (k, v) => (k, wrap(v)) }
            case _ => Nil
          }
          fromFields(fieldss)
        case _ => wrap(Json.Null)
      }

    /** Recursively merge a list of ProtoJson arrays. */
    def mergeProtoArrays(arrs: Seq[ProtoJson]): ProtoJson =
      arrs match {
        case Seq(arr) => arr
        case Seq(_, _, _*) =>
          val elemss = arrs map {
            case ProtoArray(elems) => elems
            case j: Json if j.isArray => j.asArray.get.map(wrap)
            case _ => Nil
          }
          elemss.transpose.map(mergeProtoJson) match {
            case Nil => wrap(Json.Null)
            case elems => fromValues(elems)
          }
        case _ => wrap(Json.Null)
      }

    /** Recursively merge a list of ProtoJson fields. */
    def mergeFields(fields: Seq[(String, ProtoJson)]): Seq[(String, ProtoJson)] = {
      def hasDuplicates[T](xs: Seq[(String, T)]): Boolean =
        xs match {
          case Seq(_, _, _*) =>
            val seen = mutable.HashSet.empty[String]
            xs.exists { case (k, _) => !seen.add(k) }
          case _ => false
        }

      if (!hasDuplicates(fields)) fields
      else {
        val groupedFields = fields.groupMap(_._1)(_._2).view.mapValues(mergeProtoJson).toMap
        fields.foldLeft((Set.empty[String], List.empty[(String, ProtoJson)])) {
          case ((seen, acc), (fieldName, _)) =>
            if (seen.contains(fieldName)) (seen, acc)
            else (seen + fieldName, (fieldName, groupedFields(fieldName)) :: acc)
        }._2.reverse
      }
    }

    @deprecated("Use mergeProtoObjects or mergeProtoJson instead", "0.18.1")
    def mergeObjects(objs: List[ProtoJson]): ProtoJson =
      mergeProtoObjects(objs)

    @deprecated("Use mergeProtoJson instead", "0.18.1")
    def mergeJson(objs: List[Json]): Json =
      mergeProtoJson(objs.asInstanceOf[List[ProtoJson]]).asInstanceOf[Json]

    // Combine a list of ProtoJson results, collecting all errors on the left and preserving
    // the order and number of elements by inserting Json Nulls for Lefts.
    def combineResults(ress: List[Result[ProtoJson]]): Result[List[ProtoJson]] =
      Result.combineAllWithDefault(ress, ProtoJson.fromJson(Json.Null))

    private def wrap(j: AnyRef): ProtoJson = j.asInstanceOf[ProtoJson]
  }

  import ProtoJson._

  /**
   * Complete a possibly partial result.
   *
   * Completes a single possibly partial result as described for
   * `completeAll`.
   */
  def complete[F[_]: Monad](pj: ProtoJson): F[Result[Json]] =
    pj match {
      case j: Json => Result(j).pure[F]
      case _ => completeAll[F](List(pj)).map(_.map(_.head)) // result is 1:1 with the argument, so head is safe
    }

  /** Complete a collection of possibly deferred results.
   *
   *  Each result is completed by locating any subtrees which have been
   *  deferred or delegated to some other component interpreter in an
   *  overall composite interpreter. Deferred subtrees are gathered,
   *  grouped by their associated interpreter and then evaluated in
   *  batches. The results of these batch evaluations are then
   *  completed in a subsequent stage recursively until the results are
   *  fully evaluated or yield errors.
   *
   *  Complete results are substituted back into the corresponding
   *  enclosing Json.
   *
   *  Errors are aggregated across all the results and are accumulated
   *  on the `Left` of the result.
   */
  def completeAll[F[_]: Monad](pjs: List[ProtoJson]): F[Result[List[Json]]] = {
    def gatherDeferred(pj: ProtoJson): List[DeferredJson] = {
      @tailrec
      def loop(pending: Chain[ProtoJson], acc: List[DeferredJson]): List[DeferredJson] =
        pending.uncons match {
          case None => acc
          case Some((hd, tl)) => (hd: @unchecked) match {
            case _: Json              => loop(tl, acc)
            case d: DeferredJson      => loop(tl, d :: acc)
            case ProtoObject(fields)  => loop(Chain.fromSeq(fields.map(_._2)) ++ tl, acc)
            case ProtoArray(elems)    => loop(Chain.fromSeq(elems) ++ tl, acc)
            case ProtoSelect(elem, _) => loop(elem +: tl, acc)
          }
        }

      pj match {
        case _: Json => Nil
        case _ => loop(Chain.one(pj), Nil)
      }
    }

    def scatterResults(pj: ProtoJson, subst: mutable.Map[DeferredJson, Json]): Json = {
      def loop(pj: ProtoJson): Json =
        (pj: @unchecked) match {
          case p: Json         => p
          case d: DeferredJson => subst(d)
          case ProtoObject(fields) =>
            val fields0 = fields.map { case (label, pvalue) => (label, loop(pvalue)) }
            Json.fromFields(fields0)
          case ProtoArray(elems) =>
            val elems0 = elems.map(loop)
            Json.fromValues(elems0)
          case ProtoSelect(elem, fieldName) =>
            loop(elem).asObject.flatMap(_(fieldName)).getOrElse(Json.Null)
        }

      loop(pj)
    }

    val batchedEffects =
      pjs.flatMap(gatherDeferred).asInstanceOf[List[EffectJson[F]]].groupMap(ej => (ej.mapping, ej.handler))(identity).toList

    (for {
      batchedResults <-
        batchedEffects.traverse {
          case ((mapping, handler), batch) =>
            val queries = batch.map(e => (e.query, e.cursor))
            for {
              pnext <-
                handler match {
                  case None =>
                    ResultT(mapping.combineAndRun(queries))
                  case Some(handler) =>
                    for {
                      cs    <- ResultT(handler.runEffects(queries))
                      conts <- ResultT(queries.traverse { case (q, _) => Query.extractChild(q).toResultOrError("Continuation query has the wrong shape") }.pure[F])
                      res   <- ResultT(combineResults((conts, cs).parMapN { case (query, cursor) => mapping.interpreter.runValue(query, cursor.tpe, cursor) }).pure[F])
                    } yield res
                }
              next  <- ResultT(completeAll[F](pnext))
            } yield batch.zip(next)
        }
    } yield {
      val subst = {
        val m = new java.util.IdentityHashMap[DeferredJson, Json]
        Monoid.combineAll(batchedResults).foreach { case (d, j) => m.put(d, j) }
        m.asScala
      }
      pjs.map(pj => scatterResults(pj, subst))
    }).value
  }
}
