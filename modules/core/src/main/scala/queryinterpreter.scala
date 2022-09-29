// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._

import cats.Monoid
import cats.data.{ Chain, Ior, IorT, NonEmptyChain }
import cats.implicits._
import fs2.Stream
import io.circe.Json
import io.circe.syntax._

import Cursor.{Context, Env, ListTransformCursor}
import Query._
import QueryInterpreter.{ mkErrorResult, ProtoJson }
import ProtoJson._
import edu.gemini.grackle.Result

class QueryInterpreter[F[_]](mapping: Mapping[F]) {

  /** Interpret `query` with expected type `rootTpe`.
   *
   *  The query is fully interpreted, including deferred or staged
   *  components.
   *
   *  The resulting Json value should include standard GraphQL error
   *  information in the case of failure.
   */
  def run(query: Query, rootTpe: Type, env: Env): Stream[F,Json] =
    runRoot(query, rootTpe, env).map(QueryInterpreter.mkResponse)

  /** Interpret `query` with expected type `rootTpe`.
   *
   *  The query is fully interpreted, including deferred or staged
   *  components.
   *
   *  Errors are accumulated on the `Left` of the result.
   */
  def runRoot(query: Query, rootTpe: Type, env: Env): Stream[F,Result[Json]] = {
    import mapping.RootEffect

    case class PureQuery(query: Query)
    case class EffectfulQuery(query: Query, rootEffect: RootEffect)

    val rootContext = Context(rootTpe)

    val (effectfulQueries, pureQueries) = ungroup(query).partitionMap { query =>
      (for {
        rootName <- Query.rootName(query)
        re       <- mapping.rootEffect(rootContext, rootName._1)
      } yield Left(EffectfulQuery(query, re))).getOrElse(Right(PureQuery(query)))
    }

    val mergedResults: Stream[F,Result[ProtoJson]] =
      if(mapping.schema.subscriptionType.map(_ =:= rootTpe).getOrElse(false)) {
        (effectfulQueries, pureQueries) match {
          case (List(EffectfulQuery(query, RootEffect(_, effect))), Nil) =>
            effect(query, rootTpe, env.addFromQuery(query)).map(_.flatMap {
              case (q, c) => runValue(q, rootTpe, c)
            })
          case _ =>
            Result.failure("Only one root field permitted for subscriptions").pure[Stream[F, *]]
        }
      } else {
        val pureResults: Stream[F,Result[ProtoJson]] =
          if(pureQueries.isEmpty) Stream.empty
          else {
            val (introQueries, nonIntroQueries) = pureQueries.partitionMap {
              case PureQuery(i: Introspect) => Left(i)
              case PureQuery(other) => Right(other)
            }

            val introResults =
              Stream.emits[F, Result[ProtoJson]](
                introQueries.flatMap {
                  case Introspect(schema, query) =>
                    val interp = Introspection.interpreter(schema)
                    interp.runRootValue(query, Introspection.schema.queryType, env).compile.toList // this is Stream[Id, *] so we can toList it
                })

            val nonIntroResults =
              nonIntroQueries match {
                case Nil => Stream.empty
                case List(q) => runRootValue(q, rootTpe, env)
                case qs => runRootValue(Group(qs), rootTpe, env)
              }

            introResults ++ nonIntroResults
          }

        val effectfulResults: Stream[F,Result[ProtoJson]] =
          if(effectfulQueries.isEmpty) Stream.empty
          else {
            effectfulQueries.foldMap {
              case EffectfulQuery(query, RootEffect(_, effect)) =>
                effect(query, rootTpe, env.addFromQuery(query)).map(_.flatMap {
                  case (q, c) => runValue(q, rootTpe, c)
                })
            }
          }

        (pureResults ++ effectfulResults).reduceSemigroup.map {
          case Ior.Left(errs) => Ior.Both(errs, ProtoJson.fromJson(Json.Null))
          case other => other
        }
      }

    (for {
      pvalue <- IorT(mergedResults)
      value  <- IorT(QueryInterpreter.complete[F](pvalue))
    } yield value).value
  }

  /** Interpret `query` with expected type `rootTpe`.
   *
   *  At most one stage will be run and the result may contain deferred
   *  components.
   *
   *  Errors are accumulated on the `Left` of the result.
   */
  def runRootValue(query: Query, rootTpe: Type, env: Env): Stream[F, Result[ProtoJson]] =
    (for {
      qc       <- IorT(Stream.eval(mapping.defaultRootCursor(query, rootTpe, env)))
      value    <- IorT(runValue(qc._1, rootTpe, qc._2).pure[Stream[F,*]])
    } yield value).value

  /** Interpret multiple queries with respect to their expected types.
   *
   *  Each query is interpreted with respect to the expected type it is
   *  paired with. The result list is aligned with the argument list
   *  query list. For each query at most one stage will be run and the
   *  corresponding result may contain deferred components.
   *
   *  Errors are aggregated across all the argument queries and are
   *  accumulated on the `Left` of the result.
   *
   *  This method is typically called at the end of a stage to evaluate
   *  deferred subqueries in the result of that stage. These will be
   *  grouped by and passed jointly to the responsible interpreter in
   *  the next stage using this method. Interpreters which are able
   *  to benefit from combining queries may do so by overriding this
   *  method to implement their specific combinging logic.
   */
  def runRootValues(queries: List[(Query, Type, Env)]): Stream[F, (Chain[Problem], List[ProtoJson])] =
    queries.traverse((runRootValue _).tupled).map { rs =>
      (rs.foldLeft((Chain.empty[Problem], List.empty[ProtoJson])) {
        case ((errors, elems), elem) =>
          elem match {
            case Ior.Left(errs) => (errs.toChain ++ errors, ProtoJson.fromJson(Json.Null) :: elems)
            case Ior.Right(elem) => (errors, elem :: elems)
            case Ior.Both(errs, elem) => (errs.toChain ++ errors, elem :: elems)
          }
      }).fmap(_.reverse)
    }

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
      mkErrorResult(s"Mismatched query and cursor type in runFields: $tpe ${cursor.tpe}")
    else {
      (query, tpe.dealias) match {
        case (Narrow(tp1, child), _) =>
          if (!cursor.narrowsTo(tp1)) Nil.rightIor
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
              List((resultName, ProtoJson.fromJson(Json.fromString(name)))).rightIor
            case None =>
              mkErrorResult(s"'__typename' cannot be applied to non-selectable type '$tpe'")
          }

        case (PossiblyRenamedSelect(sel, resultName), NullableType(tpe)) =>
          cursor.asNullable.sequence.map { rc =>
            for {
              c      <- rc
              fields <- runFields(sel, tpe, c)
            } yield fields
          }.getOrElse(List((resultName, ProtoJson.fromJson(Json.Null))).rightIor)

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
                case None => 0.rightIor
                case Some(c1) =>
                  if (c1.isList) c1.asList(Iterator).map(_.size)
                  else 1.rightIor
              }
            else if (c0.isList) c0.asList(Iterator).map(_.size)
            else 1.rightIor
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
          mkErrorResult(s"runFields failed: { ${query.render} } $tpe")
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
        case None => cs.rightIor
        case Some(f) =>
          val cs0 = cs.toSeq
          val tc = ListTransformCursor(parent, cs0.size, cs0)
          f(tc).flatMap(_.asList(Iterator))
      }

    def applyOps(cursors: Iterator[Cursor]): Result[(Query, Iterator[Cursor])] = {
      query0 match {
        case FilterOrderByOffsetLimit(pred, selections, offset, limit, child) =>
          val filtered =
            pred.map { p =>
              cursors.filter { c =>
                p(c) match {
                  case left@Ior.Left(_) => return left
                  case Ior.Right(c) => c
                  case Ior.Both(_, c) => c
                }
              }
            }.getOrElse(cursors)
          val sorted = selections.map(OrderSelections(_).order(filtered.toSeq).iterator).getOrElse(filtered)
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
          return mkErrorResult(s"Mismatched query and cursor type in runList: $tpe ${c.tpe}")

        runValue(child, tpe, c) match {
          case left@Ior.Left(_) => return left
          case Ior.Right(v) => builder.addOne(v)
          case Ior.Both(ps, v) =>
            builder.addOne(v)
            problems = problems.concat(ps.toChain)
        }
      }

      def mkResult(j: ProtoJson): Result[ProtoJson] =
        NonEmptyChain.fromChain(problems).map(neps => Ior.Both(neps, j)).getOrElse(j.rightIor)

      if (!unique) mkResult(ProtoJson.fromValues(builder.result()))
      else {
        val size = builder.knownSize
        if (size == 1) mkResult(builder.result()(0))
        else if (size == 0) {
          if(nullable) mkResult(ProtoJson.fromJson(Json.Null))
          else mkErrorResult(s"No match")
        } else mkErrorResult(s"Multiple matches")
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
      mkErrorResult(s"Mismatched query and cursor type in runValue: $tpe ${cursor.tpe}")
    else {
      def mkResult[T](ot: Option[T]): Result[T] = ot match {
        case Some(t) => t.rightIor
        case None => mkErrorResult(s"Join continuation has unexpected shape")
      }

      (query, tpe.dealias) match {
        case (Environment(childEnv: Env, child: Query), tpe) =>
          runValue(child, tpe, cursor.withEnv(childEnv))

        case (Wrap(_, Component(_, _, _)), ListType(tpe)) =>
          // Keep the wrapper with the component when going under the list
          cursor.asList(Iterator).map { ic =>
            val builder = Vector.newBuilder[ProtoJson]
            builder.sizeHint(ic.knownSize)
            while(ic.hasNext) {
              val c = ic.next()
              runValue(query, tpe, c) match {
                case Ior.Right(v) => builder.addOne(v)
                case left => return left
              }
            }
            ProtoJson.fromValues(builder.result())
          }

        case (Wrap(_, Defer(_, _, _)), _) if cursor.isNull =>
          ProtoJson.fromJson(Json.Null).rightIor

        case (Wrap(fieldName, child), _) =>
          for {
            pvalue <- runValue(child, tpe, cursor)
          } yield ProtoJson.fromFields(List((fieldName, pvalue)))

        case (Component(mapping, join, PossiblyRenamedSelect(child, resultName)), _) =>
          val interpreter = mapping.interpreter
          join(cursor, child).flatMap {
            case Group(conts) =>
              conts.traverse { case cont =>
                for {
                  componentName <- mkResult(rootName(cont).map(nme => nme._2.getOrElse(nme._1)))
                } yield
                  ProtoJson.select(
                    ProtoJson.staged(interpreter, cont, mapping.schema.queryType, cursor.fullEnv),
                    componentName
                  )
              }.map(ProtoJson.fromValues)

            case cont =>
              for {
                renamedCont <- mkResult(renameRoot(cont, resultName))
              } yield ProtoJson.staged(interpreter, renamedCont, mapping.schema.queryType, cursor.fullEnv)
          }

        case (Defer(join, child, rootTpe), _) =>
          def stage(cursor: Cursor) =
            for {
              cont <- join(cursor, child)
            } yield ProtoJson.staged(this, cont, rootTpe, cursor.fullEnv)

          if (cursor.isNullable)
            cursor.asNullable match {
              case Ior.Right(Some(c)) => stage(c)
              case _ => ProtoJson.fromJson(Json.Null).rightIor
            }
          else stage(cursor)

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
          }.getOrElse(ProtoJson.fromJson(Json.Null).rightIor)

        case (_, (_: ScalarType) | (_: EnumType)) =>
          cursor.asLeaf.map(ProtoJson.fromJson)

        case (_, (_: ObjectType) | (_: InterfaceType) | (_: UnionType)) =>
          runFields(query, tpe, cursor).map(ProtoJson.fromFields)

        case _ =>
          mkErrorResult(s"Stuck at type $tpe for ${query.render}")
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
    // A result which is deferred to the next stage or component of this interpreter.
    private[QueryInterpreter] case class StagedJson[F[_]](interpreter: QueryInterpreter[F], query: Query, rootTpe: Type, env: Env) extends DeferredJson
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
    def staged[F[_]](interpreter: QueryInterpreter[F], query: Query, rootTpe: Type, env: Env): ProtoJson =
      wrap(StagedJson(interpreter, query, rootTpe, env))

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

    private def wrap(j: AnyRef): ProtoJson = j.asInstanceOf[ProtoJson]
  }

  import ProtoJson._

  /**
   * Complete a possibly partial result.
   *
   * Completes a single possibly partial result as described for
   * `completeAll`.
   */
  def complete[F[_]](pj: ProtoJson): Stream[F,Result[Json]] =
    pj match {
      case j: Json => Result(j).pure[Stream[F, *]]
      case _ =>
        completeAll[F](List(pj)).map {
          case (errors, List(value)) =>
            NonEmptyChain.fromChain(errors) match {
              case Some(errors) => Ior.Both(errors, value)
              case None => value.rightIor
            }
          case _ =>
            mkErrorResult("completeAll yielded impossible result")
        }
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
  def completeAll[F[_]](pjs: List[ProtoJson]): Stream[F, (Chain[Problem], List[Json])] = {
    def gatherDeferred(pj: ProtoJson): List[DeferredJson] = {
      @tailrec
      def loop(pending: Chain[ProtoJson], acc: List[DeferredJson]): List[DeferredJson] =
        pending.uncons match {
          case None => acc
          case Some((hd, tl)) => hd match {
            case _: Json              => loop(tl, acc)
            case d: DeferredJson      => loop(tl, d :: acc)
            case ProtoObject(fields)  => loop(Chain.fromSeq(fields.map(_._2)) ++ tl, acc)
            case ProtoArray(elems)    => loop(Chain.fromSeq(elems) ++ tl, acc)
            case ProtoSelect(elem, _) => loop(elem +: tl, acc)
            case _                    => sys.error("impossible")
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

          case _ => sys.error("impossible")
        }

      loop(pj)
    }

    val collected = pjs.flatMap(gatherDeferred)

    val (good, bad, errors0) =
      collected.foldLeft((List.empty[(DeferredJson, QueryInterpreter[F], (Query, Type, Env))], List.empty[DeferredJson], Chain.empty[Problem])) {
        case ((good, bad, errors), d@StagedJson(interpreter, query, rootTpe, env)) =>
          ((d, interpreter.asInstanceOf[QueryInterpreter[F]], (query, rootTpe, env)) :: good, bad, errors)
      }

    val grouped = good.groupMap(_._2)(e => (e._1, e._3)).toList

    val staged =
      (grouped.traverse {
        case (i, dq) =>
          val (ds, qs) = dq.unzip
          for {
            pnext <- i.runRootValues(qs)
            next  <- completeAll[F](pnext._2)
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

  /**
   * Construct a GraphQL response from the possibly absent result `data`
   * and a collection of errors.
   */
  def mkResponse(data: Option[Json], errors: List[Problem]): Json = {
    val dataField = data.map { value => ("data", value) }.toList
    val fields =
      (dataField, errors) match {
        case (Nil, Nil)   => List(("errors", Json.fromValues(List(mkError("Invalid query").asJson))))
        case (data, Nil)  => data
        case (data, errs) => ("errors", errs.asJson) :: data
      }
    Json.fromFields(fields)
  }

  /** Construct a GraphQL response from a `Result`. */
  def mkResponse(result: Result[Json]): Json =
    mkResponse(result.right, result.left.map(_.toList).getOrElse(Nil))

  /**
   *  Construct a GraphQL error response from a `Result`, ignoring any
   *  right hand side in `result`.
   */
  def mkInvalidResponse(result: Result[Operation]): Json =
    mkResponse(None, result.left.map(_.toList).getOrElse(Nil))

  /** Construct a GraphQL error object */
  def mkError(message: String, locations: List[(Int, Int)] = Nil, path: List[String] = Nil): Problem =
    Problem(message, locations, path)

  def mkOneError(message: String, locations: List[(Int, Int)] = Nil, path: List[String] = Nil): NonEmptyChain[Problem] =
    NonEmptyChain.one(mkError(message, locations, path))

  /** Construct a GraphQL error object as the left hand side of a `Result` */
  def mkErrorResult[T](message: String, locations: List[(Int, Int)] = Nil, path: List[String] = Nil): Result[T] =
    Ior.leftNec(mkError(message, locations, path))
}
