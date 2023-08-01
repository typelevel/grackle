// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._

import cats.{Monad, Monoid}
import cats.data.{ Chain, NonEmptyChain }
import cats.implicits._
import fs2.Stream
import io.circe.Json

import syntax._
import Cursor.{Context, Env, ListTransformCursor}
import Query._
import QueryInterpreter.ProtoJson
import ProtoJson._

class QueryInterpreter[F[_]](mapping: Mapping[F]) {
  import mapping.{mkResponse, M, RootCursor, RootEffect, RootStream}

  /** Interpret `query` with expected type `rootTpe`.
   *
   *  The query is fully interpreted, including deferred or staged
   *  components.
   *
   *  The resulting Json value should include standard GraphQL error
   *  information in the case of failure.
   */
  def run(query: Query, rootTpe: Type, env: Env): Stream[F,Json] =
    runRoot(query, rootTpe, env).evalMap(mkResponse)

  /** Interpret `query` with expected type `rootTpe`.
   *
   *  The query is fully interpreted, including deferred or staged
   *  components.
   *
   *  Errors are accumulated on the `Left` of the result.
   */
  def runRoot(query: Query, rootTpe: Type, env: Env): Stream[F,Result[Json]] = {
    val rootCursor = RootCursor(Context(rootTpe), None, env)
    val mergedResults =
      if(mapping.schema.subscriptionType.map(_ =:= rootTpe).getOrElse(false))
        runRootStream(query, rootTpe, rootCursor)
      else
        Stream.eval(runRootEffects(query, rootTpe, rootCursor))

    (for {
      pvalue <- ResultT(mergedResults)
      value  <- ResultT(Stream.eval(QueryInterpreter.complete[F](pvalue)))
    } yield value).value
  }

  def runRootStream(query: Query, rootTpe: Type, rootCursor: Cursor): Stream[F, Result[ProtoJson]] =
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
        ).getOrElse(Result.failure("EffectMapping required for subscriptions").pure[Stream[F, *]])

      case _ =>
        Result.failure("Only one root selection permitted for subscriptions").pure[Stream[F, *]]
    }

  def runRootEffects(query: Query, rootTpe: Type, rootCursor: Cursor): F[Result[ProtoJson]] = {
    case class PureQuery(query: Query)
    case class EffectfulQuery(query: Query, rootEffect: RootEffect)

    val rootContext = Context(rootTpe)
    val ungrouped = ungroup(query)
    val hasRootStream =
      ungrouped.exists { root =>
        Query.rootName(root).flatMap(rootName => mapping.rootStream(rootContext, rootName._1)).isDefined
      }

    if(hasRootStream)
      Result.failure("RootStream only permitted in subscriptions").pure[F].widen
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
      (query, tpe.dealias) match {
        case (Narrow(tp1, child), _) =>
          if (!cursor.narrowsTo(tp1)) Nil.success
          else
            for {
              c      <- cursor.narrow(tp1)
              fields <- runFields(child, tp1, c)
            } yield fields

        case (Introspect(schema, PossiblyRenamedSelect(Select("__typename", Nil, Empty), resultName)), tpe: NamedType) =>
          (tpe match {
            case o: ObjectType => Some(o.name)
            case i: InterfaceType =>
              (schema.types.collectFirst {
                case o: ObjectType if o <:< i && cursor.narrowsTo(schema.ref(o.name)) => o.name
              })
            case u: UnionType =>
              (u.members.map(_.dealias).collectFirst {
                case nt: NamedType if cursor.narrowsTo(schema.ref(nt.name)) => nt.name
              })
            case _ => None
          }) match {
            case Some(name) =>
              List((resultName, ProtoJson.fromJson(Json.fromString(name)))).success
            case None =>
              Result.failure(s"'__typename' cannot be applied to non-selectable type '$tpe'")
          }

        case (PossiblyRenamedSelect(sel, resultName), NullableType(tpe)) =>
          cursor.asNullable.sequence.map { rc =>
            for {
              c      <- rc
              fields <- runFields(sel, tpe, c)
            } yield fields
          }.getOrElse(List((resultName, ProtoJson.fromJson(Json.Null))).success)

        case (PossiblyRenamedSelect(Select(fieldName, _, child), resultName), _) =>
          val fieldTpe = tpe.field(fieldName).getOrElse(ScalarType.AttributeType)
          for {
            c        <- cursor.field(fieldName, Some(resultName))
            value    <- runValue(child, fieldTpe, c)
          } yield List((resultName, value))

        case (Rename(resultName, Wrap(_, child)), tpe) =>
          runFields(Wrap(resultName, child), tpe, cursor)

        case (Wrap(fieldName, child), tpe) =>
          for {
            value <- runValue(child, tpe, cursor)
          } yield List((fieldName, value))

        case (Rename(resultName, Count(_, child)), tpe) =>
          runFields(Count(resultName, child), tpe, cursor)

        case (Count(fieldName, Select(countName, _, _)), _) =>
          cursor.field(countName, None).flatMap { c0 =>
            if (c0.isNullable)
              c0.asNullable.flatMap {
                case None => 0.success
                case Some(c1) =>
                  if (c1.isList) c1.asList(Iterator).map(_.size)
                  else 1.success
              }
            else if (c0.isList) c0.asList(Iterator).map(_.size)
            else 1.success
          }.map { value => List((fieldName, ProtoJson.fromJson(Json.fromInt(value)))) }

        case (Group(siblings), _) =>
          siblings.flatTraverse(query => runFields(query, tpe, cursor))

        case (Environment(childEnv: Env, child: Query), tpe) =>
          runFields(child, tpe, cursor.withEnv(childEnv))

        case (TransformCursor(f, child), _) =>
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
      def mkResult[T](ot: Option[T]): Result[T] = ot match {
        case Some(t) => t.success
        case None => Result.internalError(s"Join continuation has unexpected shape")
      }

      (query, tpe.dealias) match {
        case (Environment(childEnv: Env, child: Query), tpe) =>
          runValue(child, tpe, cursor.withEnv(childEnv))

        case (Wrap(_, Component(_, _, _)), ListType(tpe)) =>
          // Keep the wrapper with the component when going under the list
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
            case fail@Result.Failure(_) => fail
            case err@Result.InternalError(_) => err
            case Result.Warning(ps, _) => Result.Failure(ps)
          }

        case (Wrap(fieldName, child), _) =>
          for {
            pvalue <- runValue(child, tpe, cursor)
          } yield ProtoJson.fromFields(List((fieldName, pvalue)))

        case (Component(mapping, join, PossiblyRenamedSelect(child, resultName)), _) =>
          join(child, cursor).flatMap {
            case Group(conts) =>
              conts.traverse { case cont =>
                for {
                  componentName <- mkResult(rootName(cont).map(nme => nme._2.getOrElse(nme._1)))
                } yield
                  ProtoJson.select(
                    ProtoJson.component(mapping, cont, cursor),
                    componentName
                  )
              }.map(ProtoJson.fromValues)

            case cont =>
              for {
                renamedCont <- mkResult(renameRoot(cont, resultName))
              } yield ProtoJson.component(mapping, renamedCont, cursor)
          }

        case (Effect(handler, cont), _) =>
          ProtoJson.effect(mapping, handler.asInstanceOf[EffectHandler[F]], cont, cursor).success

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
          runFields(query, tpe, cursor).map(ProtoJson.fromFields)

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
        def combine(x: ProtoJson, y: ProtoJson): ProtoJson = ProtoJson.mergeObjects(List(x, y))
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
     */
    def fromFields(fields: Seq[(String, ProtoJson)]): ProtoJson =
      if(fields.forall(_._2.isInstanceOf[Json]))
        wrap(Json.fromFields(fields.asInstanceOf[Seq[(String, Json)]]))
      else
        wrap(ProtoObject(fields))

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

    def mergeObjects(elems: List[ProtoJson]): ProtoJson = {
      def loop(elems: List[ProtoJson], acc: List[(String, ProtoJson)]): List[(String, ProtoJson)] = elems match {
        case Nil                       => acc
        case (j: Json) :: tl =>
          j.asObject match {
            case Some(obj)             => loop(tl, acc ++ obj.keys.zip(obj.values.map(fromJson)))
            case None                  => loop(tl, acc)
          }
        case ProtoObject(fields) :: tl => loop(tl, acc ++ fields)
        case _ :: tl                   => loop(tl, acc)
      }

      elems match {
        case Nil        => wrap(Json.Null)
        case hd :: Nil  => hd
        case _          =>
          loop(elems, Nil) match {
            case Nil    => wrap(Json.Null)
            case fields => fromFields(fields)
          }
      }
    }

    def mergeJson(elems: List[Json]): Json =
      elems match {
        case Nil => Json.Null
        case List(elem) => elem
        case elems => elems.reduce(_.deepMerge(_))
      }

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
      case _ => completeAll[F](List(pj)).map(_.map(ProtoJson.mergeJson))
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
            val newFields: Seq[(String, Json)] =
              fields.flatMap { case (label, pvalue) =>
                val value = loop(pvalue)
                if (isDeferred(pvalue) && value.isObject) {
                  value.asObject.get.toList match {
                    case List((_, value)) => List((label, value))
                    case other => other
                  }
                }
                else List((label, value))
              }
            Json.fromFields(newFields)

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
                      conts <- ResultT(handler.runEffects(queries))
                      res   <- ResultT(combineResults(conts.map { case (query, cursor) => mapping.interpreter.runValue(query, cursor.tpe, cursor) }).pure[F])
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
