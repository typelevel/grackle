// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
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
import cats.data.{Chain, NonEmptyChain}
import cats.implicits._
import fs2.Stream
import io.circe.Json

import grackle.Cursor.ListTransformCursor
import grackle.Query._
import grackle.QueryInterpreter.{ProtoJson, ResponsePosition}
import grackle.QueryInterpreter.ProtoJson._
import grackle.syntax._

class QueryInterpreter[F[_]](mapping: Mapping[F]) {
  import mapping.{M, RootCursor, RootEffect, RootStream}

  /**
   * Interpret `query` with expected type `rootTpe`.
   *
   * The query is fully interpreted, including deferred or staged components.
   *
   * GraphQL errors are accumulated in the result.
   */
  def run(query: Query, rootTpe: Type, env: Env): Stream[F, Result[Json]] = {
    val rootCursor = RootCursor(Context(rootTpe), None, env)
    val mergedResults =
      if (mapping.schema.subscriptionType.exists(_ =:= rootTpe))
        runSubscription(query, rootTpe, rootCursor)
      else
        Stream.eval(runOneShot(query, rootTpe, rootCursor))

    // The `data` entry is nullable, so a null which reaches the root lands there.
    (for {
      pvalue <- ResultT(mergedResults)
      value <- ResultT(Stream.eval(QueryInterpreter.complete[F](pvalue)))
    } yield value).value.map {
      case Result.Failure(ps) => Result.Warning(ps, Json.Null)
      case other => other
    }
  }

  /**
   * Run a subscription query yielding a stream of results.
   */
  def runSubscription(
      query: Query,
      rootTpe: Type,
      rootCursor: Cursor): Stream[F, Result[ProtoJson]] =
    ungroup(query) match {
      case Nil => Result(ProtoJson.fromJson(Json.Null)).pure[Stream[F, *]]
      case List(root) =>
        (
          for {
            rootName <- Query.rootName(root)
            RootStream(fieldName, effect) <- mapping
              .rootStream(Context(rootTpe), rootName._1)
              .orElse(mapping.rootEffect(Context(rootTpe), rootName._1).map(_.toRootStream))
          } yield effect(root, rootTpe / fieldName, rootCursor.fullEnv.addFromQuery(root))
            .map(_.flatMap { // TODO Rework in terms of cursor
              case (q, c) => runValue(q, rootTpe, c)
            })
        ).getOrElse(
          Result.internalError("EffectMapping required for subscriptions").pure[Stream[F, *]])

      case _ =>
        Result
          .internalError("Only one root selection permitted for subscriptions")
          .pure[Stream[F, *]]
    }

  /**
   * Run a non-subscription query yielding a single result.
   */
  def runOneShot(query: Query, rootTpe: Type, rootCursor: Cursor): F[Result[ProtoJson]] = {
    case class PureQuery(query: Query)
    case class EffectfulQuery(query: Query, rootEffect: RootEffect)

    val rootContext = Context(rootTpe)
    val ungrouped = ungroup(query)
    val hasRootStream =
      ungrouped.exists { root =>
        Query
          .rootName(root)
          .flatMap(rootName => mapping.rootStream(rootContext, rootName._1))
          .isDefined
      }

    if (hasRootStream)
      Result.internalError("RootStream only permitted in subscriptions").pure[F].widen
    else {
      val (effectfulQueries, pureQueries) = ungrouped.partitionMap { query =>
        (for {
          rootName <- Query.rootName(query)
          re <- mapping.rootEffect(rootContext, rootName._1)
        } yield Left(EffectfulQuery(query, re))).getOrElse(Right(PureQuery(query)))
      }

      val pureResults: F[List[Result[ProtoJson]]] =
        if (pureQueries.isEmpty) Nil.pure[F].widen
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
        if (effectfulQueries.isEmpty) Nil.pure[F].widen
        else {
          effectfulQueries.traverse {
            case EffectfulQuery(query, RootEffect(fieldName, effect)) =>
              effect(query, rootTpe / fieldName, rootCursor.fullEnv.addFromQuery(query))
                .map(_.flatMap { // TODO Rework in terms of cursor
                  case (q, c) => runValue(q, rootTpe, c)
                })
          }
        }

      for {
        pr <- pureResults
        er <- effectfulResults
      } yield (pr ++ er) match {
        case Nil => Result(ProtoJson.fromJson(Json.Null))
        case List(r) => r
        case hd :: tl =>
          tl.foldLeft(hd) { case (acc, elem) => acc |+| elem }
      }
    }
  }

  /**
   * Interpret `query` with expected type `rootTpe`.
   *
   * At most one stage will be run and the result may contain deferred components.
   *
   * Errors are accumulated on the `Left` of the result.
   */
  def runRootValue(query: Query, rootTpe: Type, parentCursor: Cursor): F[Result[ProtoJson]] =
    (for {
      qc <- ResultT(mapping.defaultRootCursor(query, rootTpe, Some(parentCursor)))
      value <- ResultT(runValue(qc._1, rootTpe, qc._2).pure[F])
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
   * Marks `value` when it stands at a non-null response position of type `tpe`.
   */
  private def atPosition(value: ProtoJson, tpe: Type): ProtoJson =
    if (tpe.isNullable) value else ProtoJson.nonNull(value)

  /**
   * Handles a failure of the field `name` at the position `pos` as a field error: the problems
   * carry the path of `pos`, and a nullable field completes as null.
   *
   * @see
   *   https://spec.graphql.org/September2025/#sec-Handling-Field-Errors
   */
  private def fieldError(tpe: Type, pos: ResponsePosition, name: String)(
      res: Result[List[(String, ProtoJson)]]): Result[List[(String, ProtoJson)]] =
    res.atPath(pos.path) match {
      case Result.Failure(ps) if tpe.isNullable =>
        Result.Warning(ps, List((name, ProtoJson.fromJson(Json.Null))))
      case other => other
    }

  /**
   * Interpret `query` against `cursor`, yielding a collection of fields.
   *
   * If the query is valid, the field subqueries will all be valid fields of the enclosing type
   * `tpe` and the resulting fields may be used to build a Json object of type `tpe`. If the
   * query is invalid errors will be returned on the left hand side of the result.
   *
   * `path` is the response position of the enclosing object.
   */
  def runFields(
      query: Query,
      tpe: Type,
      cursor: Cursor,
      path: ResponsePosition = ResponsePosition.root): Result[List[(String, ProtoJson)]] =
    if (!cursorCompatible(tpe, cursor.tpe))
      Result.internalError(s"Mismatched query and cursor type in runFields: $tpe ${cursor.tpe}")
    else {
      query match {
        case g: Group if groupWithTypeCase(g) =>
          ungroup(g)
            .flatTraverse(query => runFields(query, tpe, cursor, path))
            .map(fs => mergeFields(fs).toList)

        case Group(siblings) =>
          siblings.flatTraverse(query => runFields(query, tpe, cursor, path))

        case Introspect(schema, s @ Select("__typename", _, Empty)) if tpe.isNamed =>
          val fail =
            Result.failure(s"'__typename' cannot be applied to non-selectable type '$tpe'")
          def mkTypeNameFields(name: String) =
            List((s.resultName, ProtoJson.fromJson(Json.fromString(name)))).success
          def mkTypeNameFieldsOrFail(name: Option[String]) =
            name.map(mkTypeNameFields).getOrElse(fail)

          tpe.dealias match {
            case o: ObjectType => mkTypeNameFields(o.name)
            case i: InterfaceType =>
              schema
                .implementations(i)
                .collectFirstSomeM { o =>
                  cursor.narrowsTo(schema.uncheckedRef(o)).ifF(Some(o.name), None)
                }
                .flatMap(mkTypeNameFieldsOrFail)
            case u: UnionType =>
              u.members
                .map(_.dealias)
                .collectFirstSomeM { nt =>
                  cursor.narrowsTo(schema.uncheckedRef(nt)).ifF(Some(nt.name), None)
                }
                .flatMap(mkTypeNameFieldsOrFail)
            case _ => fail
          }

        case sel: Select if tpe.isNullable =>
          cursor
            .asNullable
            .sequence
            .map { rc =>
              for {
                c <- rc
                fields <- runFields(sel, tpe, c, path)
              } yield fields
            }
            .getOrElse(List((sel.resultName, ProtoJson.fromJson(Json.Null))).success)

        case sel @ Select(fieldName, _, Count(Select(countName, _, _))) =>
          def size(c: Cursor): Result[Int] =
            if (c.isList) c.asList(Iterator).map(_.size)
            else 1.success

          val fieldTpe = tpe.field(fieldName).getOrElse(ScalarType.AttributeType)
          val fieldPos = path.field(sel.resultName)
          fieldError(fieldTpe, fieldPos, sel.resultName) {
            for {
              c0 <- cursor.field(countName, None)
              count <-
                if (c0.isNullable) c0.asNullable.flatMap(_.map(size).getOrElse(0.success))
                else size(c0)
            } yield List((sel.resultName, ProtoJson.fromJson(Json.fromInt(count))))
          }

        case sel @ Select(fieldName, _, Effect(handler, cont)) =>
          val fieldTpe = tpe.field(fieldName).getOrElse(ScalarType.AttributeType)
          val fieldPos = path.field(sel.resultName)
          val value =
            ProtoJson.effect(
              mapping,
              handler.asInstanceOf[EffectHandler[F]],
              cont,
              cursor,
              fieldPos)
          List((sel.resultName, atPosition(value, fieldTpe))).success

        case sel @ Select(fieldName, resultName, child) =>
          val fieldTpe = tpe.field(fieldName).getOrElse(ScalarType.AttributeType)
          val fieldPos = path.field(sel.resultName)
          fieldError(fieldTpe, fieldPos, sel.resultName) {
            for {
              c <- cursor.field(fieldName, resultName)
              value <- runValue(child, fieldTpe, c, fieldPos)
            } yield List((sel.resultName, atPosition(value, fieldTpe)))
          }

        case Narrow(tp1, child) =>
          cursor.narrowsTo(tp1).flatMap { n =>
            if (!n) Nil.success
            else
              for {
                c <- cursor.narrow(tp1)
                fields <- runFields(child, tp1, c, path)
              } yield fields
          }

        case c @ Component(_, _, cont) =>
          rootName(cont).toResultOrError("Join continuation has unexpected shape").flatMap {
            case (fieldName, alias) =>
              val componentName = alias.getOrElse(fieldName)
              val fieldTpe = tpe.field(fieldName).getOrElse(ScalarType.AttributeType)
              val fieldPos = path.field(componentName)
              runValue(c, tpe, cursor, fieldPos).map { value =>
                List(
                  (componentName, atPosition(ProtoJson.select(value, componentName), fieldTpe)))
              }
          }

        case Environment(childEnv: Env, child: Query) =>
          runFields(child, tpe, cursor.withEnv(childEnv), path)

        case TransformCursor(f, child) =>
          for {
            ct <- f(cursor)
            fields <- runFields(child, tpe, ct, path)
          } yield fields

        case _ =>
          Result.internalError(s"runFields failed: { ${query.render} } $tpe")
      }
    }

  def runList(
      query: Query,
      tpe: Type,
      parent: Cursor,
      unique: Boolean,
      nullable: Boolean,
      path: ResponsePosition = ResponsePosition.root): Result[ProtoJson] = {
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
            if (pred.isEmpty && selections.isEmpty) cursors
            else {
              val cs = cursors.toSeq
              val filtered =
                pred match {
                  case Some(p) =>
                    cs.filterA(p(_)) match {
                      case err @ Result.InternalError(_) => return err
                      case fail @ Result.Failure(_) => return fail
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
            case (Some(off), Some(lim)) => sorted.slice(off, off + lim)
          }
          transformElems(sliced).map(cs => (child, cs))
        case other =>
          transformElems(cursors).map(cs => (other, cs))
      }
    }

    def mkResult(child: Query, ic: Iterator[Cursor]): Result[ProtoJson] = {
      val builder = Vector.newBuilder[ProtoJson]
      var problems = Chain.empty[Problem]
      var index = 0
      builder.sizeHint(ic.knownSize)

      // A unique list yields one value, which stands at the position of the list itself.
      def elemPosition(i: Int): ResponsePosition = if (unique) path else path.index(i)
      def markElem(v: ProtoJson): ProtoJson = if (unique) v else atPosition(v, tpe)

      while (ic.hasNext) {
        val c = ic.next()
        if (!cursorCompatible(tpe, c.tpe))
          return Result.internalError(
            s"Mismatched query and cursor type in runList: $tpe ${c.tpe}")

        val elemPos = elemPosition(index)
        index += 1

        runValue(child, tpe, c, elemPos) match {
          case err: Result.InternalError => return err
          // A nullable element completes as null, so the other elements survive.
          case Result.Failure(ps) if !unique && tpe.isNullable =>
            val elemPath = elemPos.path
            builder.addOne(ProtoJson.fromJson(Json.Null))
            problems = problems.concat(ps.map(_.atPath(elemPath)).toChain)
          case fail: Result.Failure => return fail
          case Result.Success(v) =>
            builder.addOne(markElem(v))
          case Result.Warning(ps, v) =>
            builder.addOne(markElem(v))
            problems = problems.concat(ps.toChain)
        }
      }

      def mkResult(j: ProtoJson): Result[ProtoJson] =
        NonEmptyChain
          .fromChain(problems)
          .map(neps => Result.Warning(neps, j))
          .getOrElse(j.success)

      if (!unique) mkResult(ProtoJson.fromValues(builder.result()))
      else {
        val size = builder.knownSize
        if (size == 1) mkResult(builder.result()(0))
        else if (size == 0) {
          if (nullable) mkResult(ProtoJson.fromJson(Json.Null))
          else Result.internalError(s"No match")
        } else Result.internalError(s"Multiple matches")
      }
    }

    for {
      cursors <- parent.asList(Iterator)
      ccs <- applyOps(cursors)
      (child, cs) = ccs
      res <- mkResult(child, cs)
    } yield res
  }

  /**
   * Interpret `query` against `cursor` with expected type `tpe`.
   *
   * If the query is invalid errors will be returned on the left hand side of the result.
   *
   * `path` is the response position of the value.
   */
  def runValue(
      query: Query,
      tpe: Type,
      cursor: Cursor,
      path: ResponsePosition = ResponsePosition.root): Result[ProtoJson] = {
    if (!cursorCompatible(tpe, cursor.tpe))
      Result.internalError(s"Mismatched query and cursor type in runValue: $tpe ${cursor.tpe}")
    else {
      (query, tpe.dealias) match {
        case (Environment(childEnv: Env, child: Query), tpe) =>
          runValue(child, tpe, cursor.withEnv(childEnv), path)

        case (Component(_, _, _), ListType(tpe)) =>
          cursor.asList(Iterator) match {
            case Result.Success(ic) =>
              val builder = Vector.newBuilder[ProtoJson]
              var index = 0
              builder.sizeHint(ic.knownSize)
              while (ic.hasNext) {
                val c = ic.next()
                val elemPos = path.index(index)
                index += 1
                runValue(query, tpe, c, elemPos) match {
                  case Result.Success(v) => builder.addOne(v)
                  case notRight => return notRight
                }
              }
              ProtoJson.fromValues(builder.result()).success
            case Result.Warning(ps, _) => Result.Failure(ps)
            case fail @ Result.Failure(_) => fail
            case err @ Result.InternalError(_) => err
          }

        case (Component(mapping, join, child), _) =>
          join(child, cursor).flatMap {
            case Group(conts) =>
              for {
                childName <- resultName(child).toResultOrError(
                  "Join child has unexpected shape")
                elems <- conts.zipWithIndex.traverse {
                  case (cont, index) =>
                    for {
                      componentName <- resultName(cont).toResultOrError(
                        "Join continuation has unexpected shape")
                    } yield ProtoJson.select(
                      ProtoJson.component(mapping, cont, cursor, path.index(index)),
                      componentName
                    )
                }
              } yield ProtoJson.fromDisjointFields(
                List(childName -> ProtoJson.fromValues(elems))
              )

            case cont =>
              for {
                renamedCont <- alignResultName(child, cont).toResultOrError(
                  "Join continuation has unexpected shape")
              } yield ProtoJson.component(mapping, renamedCont, cursor, path)
          }

        case (Unique(child), _) =>
          cursor
            .preunique
            .flatMap(c => runList(child, tpe.nonNull, c, true, tpe.isNullable, path))

        case (_, ListType(tpe)) =>
          runList(query, tpe, cursor, false, false, path)

        case (TransformCursor(f, child), _) =>
          for {
            ct <- f(cursor)
            value <- runValue(child, tpe, ct, path)
          } yield value

        case (_, NullableType(tpe)) =>
          cursor
            .asNullable
            .sequence
            .map { rc =>
              for {
                c <- rc
                value <- runValue(query, tpe, c, path)
              } yield value
            }
            .getOrElse(ProtoJson.fromJson(Json.Null).success)

        case (_, _: ScalarType | _: EnumType) =>
          cursor.asLeaf.map(ProtoJson.fromJson)

        case (_, _: ObjectType | _: InterfaceType | _: UnionType) =>
          runFields(query, tpe, cursor, path).map(ProtoJson.fromDisjointFields)

        case _ =>
          Result.internalError(s"Stuck at type $tpe for ${query.render}")
      }
    }
  }
}

object QueryInterpreter {

  /**
   * The position of a value in the response.
   *
   * `segments` holds the path from the root of the response, in reverse order.
   *
   * @see
   *   https://spec.graphql.org/September2025/#sec-Response-Position
   */
  final case class ResponsePosition(segments: List[Problem.PathSegment]) {

    /**
     * The position of the field `name` inside this position.
     */
    def field(name: String): ResponsePosition =
      ResponsePosition(Problem.PathSegment.Name(name) :: segments)

    /**
     * The position of the list entry `index` inside this position.
     */
    def index(index: Int): ResponsePosition =
      ResponsePosition(Problem.PathSegment.Index(index) :: segments)

    /**
     * The response path of this position, from the root.
     */
    def path: List[Problem.PathSegment] = segments.reverse
  }

  object ResponsePosition {

    /**
     * The position of the `data` entry.
     */
    val root: ResponsePosition = ResponsePosition(Nil)
  }

  /**
   * Policy determining how errors arising from batches of deferred effects (effect handlers and
   * delegated components) are combined during result completion.
   *
   * When batches from multiple mappings are completed together, accumulation only applies if
   * every contributing mapping opts in: any `FailFast` mapping makes the whole completion fail
   * fast, since fail fast is a promise not to run further effects after a failure.
   *
   * Either way a failed batch is a field error: it completes as null and the response keeps its
   * `data` entry. Neither policy applies to internal errors, which abort the completion.
   */
  sealed trait EffectErrorPolicy

  object EffectErrorPolicy {

    /**
     * Stop at the first failed effect batch; subsequent batches' effects are not run.
     *
     * The failed batch and the batches which do not run all complete as null.
     */
    case object FailFast extends EffectErrorPolicy

    /**
     * Run every effect batch and accumulate errors from all of them, in document order.
     *
     * Each failed batch completes as null.
     */
    case object Accumulate extends EffectErrorPolicy

    implicit val monoidEffectErrorPolicy: Monoid[EffectErrorPolicy] =
      Monoid.instance(
        Accumulate,
        (x, y) => if (x == FailFast || y == FailFast) FailFast else Accumulate)
  }

  /**
   * Opaque type of partially constructed query results.
   *
   * Values may be fully expanded Json values, objects or arrays which not yet fully evaluated
   * subtrees, or subqueries which are deferred to the next stage or another component of a
   * composite interpreter.
   */
  type ProtoJson <: AnyRef

  object ProtoJson {
    private[QueryInterpreter] sealed trait DeferredJson
    // A result which depends on an effect and a continuation in the next stage of this or another interpreter.
    private[QueryInterpreter] case class EffectJson[F[_]](
        mapping: Mapping[F],
        handler: Option[EffectHandler[F]],
        query: Query,
        cursor: Cursor,
        position: ResponsePosition)
        extends DeferredJson
    // A partially constructed object which has at least one deferred subtree.
    private[QueryInterpreter] case class ProtoObject(fields: Seq[(String, ProtoJson)])
    // A partially constructed array which has at least one deferred element.
    private[QueryInterpreter] case class ProtoArray(elems: Seq[ProtoJson])
    // A result which will yield a selection from its child
    private[QueryInterpreter] case class ProtoSelect(elem: ProtoJson, fieldName: String)
    // A subtree at a non-null response position. A null from below propagates past it, to the
    // nearest enclosing nullable position.
    private[QueryInterpreter] case class ProtoNonNull(elem: ProtoJson)
    // A null which propagates from the position at which it stands.
    private[QueryInterpreter] case class ProtoNull()

    implicit val monoidInstance: Monoid[ProtoJson] =
      new Monoid[ProtoJson] {
        val empty: ProtoJson = fromJson(Json.Null)
        def combine(x: ProtoJson, y: ProtoJson): ProtoJson =
          ProtoJson.mergeProtoJson(List(x, y))
      }

    /**
     * Delegate `query` to the interpreter `interpreter`. When evaluated by that interpreter the
     * query will have expected type `rootTpe`.
     */
    def component[F[_]](
        mapping: Mapping[F],
        query: Query,
        cursor: Cursor,
        position: ResponsePosition = ResponsePosition.root): ProtoJson =
      wrap(EffectJson(mapping, None, query, cursor, position))

    def effect[F[_]](
        mapping: Mapping[F],
        handler: EffectHandler[F],
        query: Query,
        cursor: Cursor,
        position: ResponsePosition = ResponsePosition.root): ProtoJson =
      wrap(EffectJson(mapping, Some(handler), query, cursor, position))

    def fromJson(value: Json): ProtoJson = wrap(value)

    /**
     * Marks `pj` as a value at a non-null response position. A complete Json value holds no
     * null which propagates, so it needs no mark.
     */
    def nonNull(pj: ProtoJson): ProtoJson =
      if (pj.isInstanceOf[Json]) pj else wrap(ProtoNonNull(pj))

    /**
     * A null which propagates to the nearest enclosing nullable position.
     *
     * A failed value completes as such a null. A Json null stops at its own position instead.
     */
    val propagatingNull: ProtoJson = wrap(ProtoNull())

    /**
     * Combine possibly partial fields to create a possibly partial object.
     *
     * If all fields are complete then they will be combined as a complete Json object.
     *
     * Assumes that all fields are disjoint.
     */
    def fromDisjointFields(fields: Seq[(String, ProtoJson)]): ProtoJson =
      if (fields.forall(_._2.isInstanceOf[Json]))
        wrap(Json.fromFields(fields.asInstanceOf[Seq[(String, Json)]]))
      else
        wrap(ProtoObject(fields))

    /**
     * Combine possibly partial fields to create a possibly partial object.
     *
     * If all fields are complete then they will be combined as a complete Json object.
     */
    def fromFields(fields: Seq[(String, ProtoJson)]): ProtoJson =
      fromDisjointFields(mergeFields(fields))

    /**
     * Combine possibly partial values to create a possibly partial array.
     *
     * If all values are complete then they will be combined as a complete Json array.
     */
    def fromValues(elems: Seq[ProtoJson]): ProtoJson =
      if (elems.forall(_.isInstanceOf[Json]))
        wrap(Json.fromValues(elems.asInstanceOf[Seq[Json]]))
      else
        wrap(ProtoArray(elems))

    /**
     * Select a value from a possibly partial object.
     *
     * If the object is complete the selection will be a complete Json value.
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
     * Yields `true` if the argument contains any component or staged subtrees, false otherwise.
     */
    def isDeferred(p: ProtoJson): Boolean =
      p match {
        case _: DeferredJson => true
        case ProtoNonNull(elem) => isDeferred(elem)
        case _ => false
      }

    /**
     * Recursively merge a list of ProtoJson values.
     */
    def mergeProtoJson(elems: Seq[ProtoJson]): ProtoJson = {
      // The merge matches on the shape of a value, so the non-null marks come off first and go
      // back on the merged value.
      val marked = elems.exists(_.isInstanceOf[ProtoNonNull])
      val stripped = if (marked) elems.map(stripNonNull) else elems
      val merged =
        stripped match {
          case Seq(elem) => elem
          case Seq(_: ProtoObject, _*) => mergeProtoObjects(stripped)
          case Seq(j: Json, _*) if j.isObject => mergeProtoObjects(stripped)
          case Seq(_: ProtoArray, _*) => mergeProtoArrays(stripped)
          case Seq(j: Json, _*) if j.isArray => mergeProtoArrays(stripped)
          case Seq(hd, _*) => hd
          case _ => wrap(Json.Null)
        }
      if (marked) nonNull(merged) else merged
    }

    private def stripNonNull(pj: ProtoJson): ProtoJson =
      pj match {
        case ProtoNonNull(elem) => elem
        case other => other
      }

    /**
     * Recursively merge a list of ProtoJson objects.
     */
    def mergeProtoObjects(objs: Seq[ProtoJson]): ProtoJson =
      objs match {
        case Seq(obj) => obj
        case Seq(_, _, _*) =>
          val fieldss = objs flatMap {
            case ProtoObject(fields) => fields
            case j: Json if j.isObject =>
              j.asObject.get.toIterable.map { case (k, v) => (k, wrap(v)) }
            case _ => Nil
          }
          fromFields(fieldss)
        case _ => wrap(Json.Null)
      }

    /**
     * Recursively merge a list of ProtoJson arrays.
     */
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

    /**
     * Recursively merge a list of ProtoJson fields.
     */
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
        fields
          .foldLeft((Set.empty[String], List.empty[(String, ProtoJson)])) {
            case ((seen, acc), (fieldName, _)) =>
              if (seen.contains(fieldName)) (seen, acc)
              else (seen + fieldName, (fieldName, groupedFields(fieldName)) :: acc)
          }
          ._2
          .reverse
      }
    }

    @deprecated("Use mergeProtoObjects or mergeProtoJson instead", "0.18.1")
    def mergeObjects(objs: List[ProtoJson]): ProtoJson =
      mergeProtoObjects(objs)

    @deprecated("Use mergeProtoJson instead", "0.18.1")
    def mergeJson(objs: List[Json]): Json =
      mergeProtoJson(objs.asInstanceOf[List[ProtoJson]]).asInstanceOf[Json]

    // Combine a list of ProtoJson results, collecting all errors on the left and preserving
    // the order and number of elements by inserting nulls for the failures. A failed element
    // has no value, so its null propagates to the nearest enclosing nullable position.
    def combineResults(ress: List[Result[ProtoJson]]): Result[List[ProtoJson]] =
      Result.combineAllWithDefault(ress, propagatingNull)

    private def wrap(j: AnyRef): ProtoJson = j.asInstanceOf[ProtoJson]
  }

  import ProtoJson._

  /**
   * Complete a possibly partial result.
   *
   * Completes a single possibly partial result as described for `completeAll`.
   */
  def complete[F[_]: Monad](pj: ProtoJson): F[Result[Json]] =
    pj match {
      case j: Json => Result(j).pure[F]
      case _ =>
        completeAll[F](List(pj))
          .map(_.map(_.head)) // result is 1:1 with the argument, so head is safe
    }

  /**
   * Complete a collection of possibly deferred results.
   *
   * Each result is completed by locating any subtrees which have been deferred or delegated to
   * some other component interpreter in an overall composite interpreter. Deferred subtrees are
   * gathered, grouped by their associated interpreter and then evaluated in batches. The
   * results of these batch evaluations are then completed in a subsequent stage recursively
   * until the results are fully evaluated or yield errors.
   *
   * Complete results are substituted back into the corresponding enclosing Json.
   *
   * Errors are aggregated across all the results and are accumulated on the `Left` of the
   * result:
   *   - A failed effect batch is a field error. Its problems become warnings and its positions
   *     complete as null, so sibling data survives and the response keeps its `data` entry.
   *   - A null at a non-null position propagates to the nearest enclosing nullable position.
   *   - An internal error aborts the completion.
   */
  def completeAll[F[_]: Monad](pjs: List[ProtoJson]): F[Result[List[Json]]] =
    completeAllOrNull[F](pjs).map(_.map(_.map(_.getOrElse(Json.Null))))

  // A null which stops at a nullable position.
  private val SomeNull: Option[Json] = Some(Json.Null)

  /**
   * Complete a collection of possibly deferred results, as `completeAll`.
   *
   * A result is `None` when a null propagates past the root of that result. The `data` entry is
   * nullable, so `completeAll` turns such a result into a Json null.
   */
  private def completeAllOrNull[F[_]: Monad](
      pjs: List[ProtoJson]): F[Result[List[Option[Json]]]] = {
    // Yields deferred fields in document order.
    def gatherDeferred(pj: ProtoJson): List[DeferredJson] = {
      @tailrec
      def loop(pending: Chain[ProtoJson], acc: Chain[DeferredJson]): Chain[DeferredJson] =
        pending.uncons match {
          case None => acc
          case Some((hd, tl)) =>
            (hd: @unchecked) match {
              case _: Json => loop(tl, acc)
              case _: ProtoNull => loop(tl, acc)
              case d: DeferredJson => loop(tl, acc :+ d)
              case ProtoObject(fields) => loop(Chain.fromSeq(fields.map(_._2)) ++ tl, acc)
              case ProtoArray(elems) => loop(Chain.fromSeq(elems) ++ tl, acc)
              case ProtoSelect(elem, _) => loop(elem +: tl, acc)
              case ProtoNonNull(elem) => loop(elem +: tl, acc)
            }
        }

      pj match {
        case _: Json => Nil
        case _ => loop(Chain.one(pj), Chain.empty).toList
      }
    }

    def scatterResults(
        pj: ProtoJson,
        subst: mutable.Map[DeferredJson, Option[Json]]): Option[Json] = {
      // Yields None when a null propagates past `pj`: a position below it completed as null,
      // and no position in between is nullable.
      def loop(pj: ProtoJson): Option[Json] =
        (pj: @unchecked) match {
          case p: Json => Some(p)
          case _: ProtoNull => None
          case d: DeferredJson => subst(d)
          case ProtoNonNull(elem) => loop(elem)
          case ProtoObject(fields) =>
            fields
              .traverse { case (label, pvalue) => position(pvalue).tupleLeft(label) }
              .map(Json.fromFields)
          case ProtoArray(elems) =>
            elems.traverse(position).map(Json.fromValues)
          case ProtoSelect(elem, fieldName) =>
            loop(elem).map(_.asObject.flatMap(_(fieldName)).getOrElse(Json.Null))
        }

      // A position without a non-null mark stops the propagation with a null.
      def position(pj: ProtoJson): Option[Json] =
        if (pj.isInstanceOf[ProtoNonNull]) loop(pj) else loop(pj).orElse(SomeNull)

      loop(pj)
    }

    // We group by `(mapping, handler)`, but derive the batch ordering (and the member
    // ordering within each batch) from document order rather than from `Map` iteration
    // order, which is non-deterministic and made the accumulated error order
    // non-deterministic.
    val deferred = pjs.flatMap(gatherDeferred).asInstanceOf[List[EffectJson[F]]]
    val grouped = deferred.groupMap(ej => (ej.mapping, ej.handler))(identity)
    val batchedEffects =
      deferred.map(ej => (ej.mapping, ej.handler)).distinct.fproduct(grouped)

    def runBatch(
        mapping: Mapping[F],
        handler: Option[EffectHandler[F]],
        batch: List[EffectJson[F]]): F[Result[List[(EffectJson[F], Option[Json])]]] = {
      val queries = batch.map(e => (e.query, e.cursor))
      (for {
        pnext <-
          handler match {
            case None =>
              ResultT(mapping.combineAndRun(queries))
            case Some(handler) =>
              for {
                cs <- ResultT(handler.runEffects(queries))
                conts <- ResultT(
                  queries
                    .traverse {
                      case (q, _) =>
                        Query
                          .extractChild(q)
                          .toResultOrError("Continuation query has the wrong shape")
                    }
                    .pure[F])
                res <- ResultT.fromResult[F, List[ProtoJson]](
                  combineResults((batch, conts, cs).parMapN { (e, query, cursor) =>
                    mapping
                      .interpreter
                      .runValue(query, cursor.tpe, cursor, e.position)
                      .atPath(e.position.path)
                  }))
              } yield res
          }
        next <- ResultT(completeAllOrNull[F](pnext))
      } yield batch.zip(next)).value
    }

    val policy = deferred.map(_.mapping).distinct.foldMap(_.effectErrorPolicy)

    type Batch = ((Mapping[F], Option[EffectHandler[F]]), List[EffectJson[F]])
    type Completed = List[(EffectJson[F], Option[Json])]

    // Completes every deferred position of a batch as null. Failed and unrun batches are nulled
    // rather than dropped, so that the substitution in `scatterResults` stays total.
    def nullBatch(batch: List[EffectJson[F]]): Completed =
      batch.tupleRight(None)

    // Handles a failed batch as a field error: its problems become warnings and its positions
    // complete as null. A batch which covers exactly one position carries the path of that position.
    def batchFieldError(
        batch: List[EffectJson[F]],
        ps: NonEmptyChain[Problem]): Result[Completed] = {
      val ps0 =
        batch match {
          case List(e) =>
            val path = e.position.path
            ps.map(_.atPath(path))
          case _ => ps
        }
      Result.Warning(ps0, nullBatch(batch))
    }

    // The completions of every batch, in document order.
    val runBatches: F[List[Result[Completed]]] =
      policy match {
        case EffectErrorPolicy.FailFast =>
          def loop(
              pending: Chain[Batch],
              acc: Chain[Result[Completed]]): F[Chain[Result[Completed]]] =
            pending.uncons match {
              case None => acc.pure[F]
              case Some((((mapping, handler), batch), tl)) =>
                runBatch(mapping, handler, batch).flatMap {
                  case Result.Failure(ps) =>
                    // Stop here. This batch and the batches which do not run all complete as
                    // null.
                    val unrun = tl.map { case (_, b) => nullBatch(b).success }
                    ((acc :+ batchFieldError(batch, ps)) ++ unrun).pure[F]
                  case res if res.hasValue => loop(tl, acc :+ res)
                  // An internal error is not a field error. It aborts the completion.
                  case err => (acc :+ err).pure[F]
                }
            }
          loop(Chain.fromSeq(batchedEffects), Chain.empty).map(_.toList)
        case EffectErrorPolicy.Accumulate =>
          // Run every batch, so that the problems of *all* of them are preserved.
          batchedEffects.traverse {
            case ((mapping, handler), batch) =>
              runBatch(mapping, handler, batch).map {
                case Result.Failure(ps) => batchFieldError(batch, ps)
                case other => other
              }
          }
      }

    // No batch is left as a `Failure`, so this accumulates the problems of all batches and
    // propagates any internal error.
    val batchedResults = runBatches.map(_.parSequence)

    batchedResults.map(_.map { results =>
      val subst = {
        val m = new java.util.IdentityHashMap[DeferredJson, Option[Json]]
        Monoid.combineAll(results).foreach { case (d, j) => m.put(d, j) }
        m.asScala
      }
      pjs.map(pj => scatterResults(pj, subst))
    })
  }
}
