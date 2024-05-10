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
import scala.collection.mutable.{ Map => MMap }
import scala.reflect.ClassTag

import cats.{ApplicativeError, Id, MonadThrow}
import cats.data.{Chain, NonEmptyList, StateT}
import cats.implicits._
import fs2.{ Stream, Compiler }
import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.tpolecat.sourcepos.SourcePos
import org.tpolecat.typename._
import org.typelevel.scalaccompat.annotation._

import syntax._
import Cursor.{AbstractCursor, ProxyCursor}
import Query.EffectHandler
import QueryCompiler.{ComponentElaborator, EffectElaborator, IntrospectionLevel, SelectElaborator}
import QueryInterpreter.ProtoJson
import IntrospectionLevel._
import ValidationFailure.Severity

/**
 * Represents a mapping between a GraphQL schema and an underlying abstract data source.
 */
abstract class Mapping[F[_]] {
  implicit val M: MonadThrow[F]
  val schema: Schema
  val typeMappings: TypeMappings

  /**
    * Compile and run a single GraphQL query or mutation.
    *
    * Yields a JSON response containing the result of the query or mutation.
    */
  def compileAndRun(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, reportUnused: Boolean = true, env: Env = Env.empty)(
    implicit sc: Compiler[F,F]
  ): F[Json] =
    compileAndRunSubscription(text, name, untypedVars, introspectionLevel, reportUnused, env).compile.toList.flatMap {
      case List(j) => j.pure[F]
      case Nil     => M.raiseError(new IllegalStateException("Result stream was empty."))
      case js      => M.raiseError(new IllegalStateException(s"Result stream contained ${js.length} results; expected exactly one."))
    }

  /**
   * Compile and run a GraphQL subscription.
   *
   * Yields a stream of JSON responses containing the results of the subscription.
   */
  def compileAndRunSubscription(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, reportUnused: Boolean = true, env: Env = Env.empty): Stream[F,Json] = {
    val compiled = compiler.compile(text, name, untypedVars, introspectionLevel, reportUnused, env)
    Stream.eval(compiled.pure[F]).flatMap(_.flatTraverse(op => interpreter.run(op.query, op.rootTpe, env))).evalMap(mkResponse)
  }

  /** Combine and execute multiple queries.
   *
   *  Each query is interpreted in the context of the Cursor it is
   *  paired with. The result list is aligned with the argument
   *  query list. For each query at most one stage will be run and the
   *  corresponding result may contain deferred components.
   *
   *  Errors are aggregated across all the argument queries and are
   *  accumulated on the `Left` of the result.
   *
   *  This method is typically called at the end of a stage to evaluate
   *  deferred subqueries in the result of that stage. These will be
   *  grouped by and passed jointly to the responsible mapping in
   *  the next stage using this method. Maappongs which are able
   *  to benefit from combining queries may do so by overriding this
   *  method to implement their specific combinging logic.
   */
  def combineAndRun(queries: List[(Query, Cursor)]): F[Result[List[ProtoJson]]] =
    queries.map { case (q, c) => (q, schema.queryType, c) }.traverse((interpreter.runOneShot _).tupled).map(ProtoJson.combineResults)

  /** Yields a `Cursor` focused on the top level operation type of the query */
  def defaultRootCursor(query: Query, tpe: Type, parentCursor: Option[Cursor]): F[Result[(Query, Cursor)]] =
    Result((query, RootCursor(Context(tpe), parentCursor, Env.empty))).pure[F].widen

  /**
   * Root `Cursor` focussed on the top level operation of a query
   *
   * Construction of mapping-specific cursors is handled by delegation to
   * `mkCursorForField which is typically overridden in `Mapping` subtypes.
   */
  case class RootCursor(context: Context, parent: Option[Cursor], env: Env) extends AbstractCursor {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def focus: Any = ()

    override def hasField(fieldName: String): Boolean =
      typeMappings.fieldMapping(context, fieldName).isDefined

    override def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      mkCursorForField(this, fieldName, resultName)
  }

  /**
    * Yields a `Cursor` suitable for traversing the query result corresponding to
    * the `fieldName` child of `parent`.
    *
    * This method is typically overridden in and delegated to by `Mapping` subtypes.
    */
  def mkCursorForField(parent: Cursor, fieldName: String, resultName: Option[String]): Result[Cursor] = {
    val context = parent.context
    val fieldContext = context.forFieldOrAttribute(fieldName, resultName)

    def mkLeafCursor(focus: Any): Result[Cursor] =
      LeafCursor(fieldContext, focus, Some(parent), parent.env).success

    typeMappings.fieldMapping(context, fieldName) match {
      case Some(_ : EffectMapping) =>
        mkLeafCursor(parent.focus)
      case Some(CursorField(_, f, _, _, _)) =>
        f(parent).flatMap(res => mkLeafCursor(res))
      case _ =>
        Result.failure(s"No field '$fieldName' for type ${parent.tpe}")
    }
  }

  case class TypeMappings private (mappings: Seq[TypeMapping], unsafe: Boolean) {
    /** Yields the `TypeMapping` associated with the provided context, if any. */
    def typeMapping(context: Context): Option[TypeMapping] = {
      val nt = context.tpe.underlyingNamed
      val nme = nt.name
      singleIndex.get(nme).orElse {
        val nc = context.asType(nt)
        multipleIndex.get(nme).getOrElse(Nil).mapFilter { tm =>
          tm.predicate(nc).map(prio => (prio, tm))
        }.maxByOption(_._1).map(_._2)
      }
    }

    private val (singleIndex, multipleIndex): (MMap[String, TypeMapping], MMap[String, Seq[TypeMapping]]) = {
      val defaultLeafMappings: Seq[TypeMapping] = {
        val intTypeEncoder: Encoder[Any] =
          new Encoder[Any] {
            def apply(i: Any): Json = (i: @unchecked) match {
              case i: Int => Json.fromInt(i)
              case l: Long => Json.fromLong(l)
            }
          }

        val floatTypeEncoder: Encoder[Any] =
          new Encoder[Any] {
            def apply(f: Any): Json = (f: @unchecked) match {
              case f: Float => Json.fromFloatOrString(f)
              case d: Double => Json.fromDoubleOrString(d)
              case d: BigDecimal => Json.fromBigDecimal(d)
            }
          }

        Seq(
          LeafMapping[String](ScalarType.StringType),
          LeafMapping.DefaultLeafMapping[Any](MappingPredicate.TypeMatch(ScalarType.IntType), intTypeEncoder, typeName[Int]),
          LeafMapping.DefaultLeafMapping[Any](MappingPredicate.TypeMatch(ScalarType.FloatType), floatTypeEncoder, typeName[Float]),
          LeafMapping[Boolean](ScalarType.BooleanType),
          LeafMapping[String](ScalarType.IDType)
        )
      }

      val grouped = (mappings ++ defaultLeafMappings).groupBy(_.tpe.underlyingNamed.name)
      val (single, multiple) =
        grouped.partitionMap {
          case (nme, tms) if tms.sizeCompare(1) == 0 => Left(nme -> tms.head)
          case (nme, tms) => Right(nme -> tms)
        }
      (MMap.from(single), MMap.from(multiple))
    }

    /** Yields the `ObjectMapping` associated with the provided context, if any. */
    def objectMapping(context: Context): Option[ObjectMapping] =
      typeMapping(context).collect {
        case om: ObjectMapping => om
      }

    /** Yields the `FieldMapping` associated with `fieldName` in `context`, if any. */
    def fieldMapping(context: Context, fieldName: String): Option[FieldMapping] =
      objectMapping(context).flatMap(_.fieldMapping(fieldName)).orElse {
        context.tpe.underlyingObject match {
          case Some(ot: ObjectType) =>
            ot.interfaces.collectFirstSome(nt => fieldMapping(context.asType(nt), fieldName))
          case _ => None
        }
      }

    /** Yields the `FieldMapping` directly or ancestrally associated with `fieldName` in `context`, if any. */
    def ancestralFieldMapping(context: Context, fieldName: String): Option[FieldMapping] =
      fieldMapping(context, fieldName).orElse {
        for {
          parent <- context.parent
          fm     <- ancestralFieldMapping(parent, context.path.head)
          if fm.subtree
        } yield fm
      }

    /** Yields the `ObjectMapping` and `FieldMapping` associated with `fieldName` in `context`, if any. */
    def objectAndFieldMapping(context: Context, fieldName: String): Option[(ObjectMapping, FieldMapping)] =
      objectMapping(context).flatMap(om => om.fieldMapping(fieldName).map(fm => (om, fm))).orElse {
        context.tpe.underlyingObject match {
          case Some(ot: ObjectType) =>
            ot.interfaces.collectFirstSome(nt => objectAndFieldMapping(context.asType(nt), fieldName))
          case _ => None
        }
      }

    /** Validates these type mappings against an unfolding of the schema */
    def validate: List[ValidationFailure] = {
      val queryType = schema.schemaType.field("query").flatMap(_.nonNull.asNamed)
      val topLevelContexts = (queryType.toList ::: schema.mutationType.toList ::: schema.subscriptionType.toList).map(Context(_))
      validateRoots(topLevelContexts)
    }

    /** Validates these type mappings against an unfolding of the schema */
    @nowarn3
    private def validateRoots(rootCtxts: List[Context]): List[ValidationFailure] = {
      import TypeMappings.{MappingValidator => MV} // Bogus unused import warning with Scala 3.3.3
      import MV.{
        initialState, addSeenType, addSeenTypeMapping, addSeenFieldMapping, addProblem,
        addProblems, seenType, seenTypeMappings, seenFieldMappings
      }

      def allTypeMappings(context: Context): Seq[TypeMapping] = {
        val nt = context.tpe.underlyingNamed
        val nme = nt.name
        singleIndex.get(nme) match {
          case Some(tm) => List(tm)
          case None =>
            multipleIndex.get(nme) match {
              case None => Nil
              case Some(tms) =>
                val nc = context.asType(nt)
                tms.mapFilter { tm =>
                  tm.predicate(nc).map(prio => (prio, tm))
                } match {
                  case Seq() => Nil
                  case Seq((_, tm)) => List(tm)
                  case ptms =>
                    // At least two ...
                    val sorted = ptms.sortBy(-_._1)
                    val topPrio = sorted.head._1
                    sorted.takeWhile(_._1 == topPrio).map(_._2)
                }
            }
        }
      }

      def allPrefixedMatchContexts(ctx: Context): Seq[Context] =
        mappings.flatMap(_.predicate.continuationContext(ctx))

      def step(context: Context): MV[List[Context]] = {
        lazy val hasEnclosingSubtreeFieldMapping =
          if (context.path.isEmpty) false
          else ancestralFieldMapping(context.parent.get, context.path.head).map(_.subtree).getOrElse(false)

        def typeChecks(om: ObjectMapping): List[ValidationFailure] =
          validateTypeMapping(this, context, om) ++
            om.fieldMappings.reverse.flatMap(validateFieldMapping(this, context, om, _))

        (context.tpe.underlyingNamed.dealias, allTypeMappings(context)) match {
          case (lt@((_: ScalarType) | (_: EnumType)), List(lm: LeafMapping[_])) =>
            addSeenType(lt) *>
            addSeenTypeMapping(lm) *>
            MV.pure(Nil)

          case (lt@((_: ScalarType) | (_: EnumType)), Nil) if hasEnclosingSubtreeFieldMapping =>
            addSeenType(lt) *>
            MV.pure(Nil)

          case (lt@((_: ScalarType) | (_: EnumType)), List(om: ObjectMapping)) =>
            addSeenType(lt) *>
            addSeenTypeMapping(om) *>
            addProblem(ObjectTypeExpected(om)) *>
            MV.pure(Nil)

          case (ut: UnionType, List(om: ObjectMapping)) =>
            val vs = ut.members.map(context.asType)
            addSeenType(ut) *>
            addSeenTypeMapping(om) *>
            addProblems(typeChecks(om)) *>
            MV.pure(vs)

          case (ut: UnionType, List(lm: LeafMapping[_])) =>
            val vs = ut.members.map(context.asType)
            addSeenType(ut) *>
            addSeenTypeMapping(lm) *>
            addProblem(LeafTypeExpected(lm)) *>
            MV.pure(vs)

          case (ut: UnionType, Nil) =>
            val vs = ut.members.map(context.asType)
            addSeenType(ut) *>
            MV.pure(vs)

          case (twf: TypeWithFields, tms) if tms.sizeCompare(1) <= 0 =>
            def objectCheck(seen: Boolean) =
              (twf, tms) match {
                case (_, List(om: ObjectMapping)) =>
                  addSeenTypeMapping(om) *>
                  addProblems(typeChecks(om)).whenA(!seen)

                case (_, List(lm: LeafMapping[_])) =>
                  addSeenTypeMapping(lm) *>
                  addProblem(LeafTypeExpected(lm))

                case (_: InterfaceType, Nil) =>
                  MV.unit

                case (_, Nil) if !hasEnclosingSubtreeFieldMapping =>
                  addProblem(MissingTypeMapping(context))

                case _ =>
                  MV.unit
              }

            val implCtxts =
              twf match {
                case it: InterfaceType =>
                  schema.types.collect {
                    case ot: ObjectType if ot <:< it => context.asType(ot)
                  }
                case _ => Nil
              }

            def interfaceContext(it: NamedType): MV[Option[Context]] = {
              val v = context.asType(it)
              for {
                seen <- seenType(v.tpe.underlyingNamed)
              } yield if(seen) None else Some(v)
            }

            val fieldNames = twf.fields.map(_.name)

            def fieldCheck(fieldName: String): MV[Option[Context]] = {
              val fctx = context.forFieldOrAttribute(fieldName, None).forUnderlyingNamed
              ((ancestralFieldMapping(context, fieldName), tms) match {
                case (Some(fm), List(om: ObjectMapping)) =>
                  addSeenFieldMapping(om, fm)

                case (None, List(om: ObjectMapping)) if !hasEnclosingSubtreeFieldMapping =>
                  val field = context.tpe.fieldInfo(fieldName).get
                  addProblem(MissingFieldMapping(om, field))

                case _ => // Other errors will have been reported earlier
                  MV.unit
              }) *>
              seenType(fctx.tpe).map(if(_) None else Some(fctx))
            }

            for {
              seen    <- seenType(twf)
              _       <- addSeenType(twf)
              _       <- objectCheck(seen)
              ifCtxts <- twf.allInterfaces.traverseFilter(interfaceContext)
              fCtxts  <- fieldNames.traverseFilter(fieldCheck)
              pfCtxts <- MV.pure(if (!seen) allPrefixedMatchContexts(context) else Nil)
            } yield implCtxts ++ ifCtxts ++ fCtxts ++ pfCtxts

          case (_: TypeRef | _: InputObjectType, _) =>
            MV.pure(Nil) // Errors will have been reported earlier

          case (_, Nil) =>
            addProblem(MissingTypeMapping(context)) *>
            MV.pure(Nil)

          case (_, tms) =>
            (tms.traverse_ { // Suppress false positive follow on errors
              case om: ObjectMapping =>
                for {
                  _ <- addSeenTypeMapping(om)
                  _ <- om.fieldMappings.traverse_(fm => addSeenFieldMapping(om, fm))
                } yield ()
              case tm =>
                addSeenTypeMapping(tm)
            }) *>
            addProblem(AmbiguousTypeMappings(context, tms)) *>
            MV.pure(Nil)
        }
      }

      def validateAll(pending: List[Context]): MV[Unit] = {
        pending.tailRecM {
          case Nil =>
            MV.pure(Right(()))
          case head :: tail =>
            step(head).map(next => Left(next ::: tail))
        }
      }

      def unseenTypeMappings(seen: Set[TypeMapping]): Seq[TypeMapping] = {
        @annotation.tailrec
        def loop(pending: Seq[TypeMapping], acc: Seq[TypeMapping]): Seq[TypeMapping] =
        pending match {
          case Seq(om: ObjectMapping, tail @ _*) =>
            if (seen(om) || om.fieldMappings.forall { case _: Delegate => true ; case _ => false })
              loop(tail, acc)
            else
              loop(tail, om +: acc)

          case Seq(tm,  tail @ _*) if seen(tm) =>
            loop(tail, acc)

          case Seq(tm,  tail @ _*) =>
            loop(tail, tm +: acc)

          case _ => acc.reverse
        }

        loop(mappings, Nil)
      }

      def refChecks(tm: TypeMapping): MV[Unit] = {
        addProblem(ReferencedTypeDoesNotExist(tm)).whenA(!tm.tpe.exists) *>
        (tm match {
          case om: ObjectMapping if !om.tpe.dealias.isUnion =>
            for {
              sfms  <- seenFieldMappings(om)
              usfms =  om.fieldMappings.filterNot { case _: Delegate => true ; case fm => fm.hidden || sfms(fm) }
              _     <- usfms.traverse_(fm => addProblem(UnusedFieldMapping(om, fm)))
            } yield ()
          case _ =>
            MV.unit
        })
      }

      val res =
        for {
          _    <- addProblem(MissingTypeMapping(Context(schema.uncheckedRef("Query")))).whenA(schema.schemaType.field("query").isEmpty)
          _    <- validateAll(rootCtxts)
          seen <- seenTypeMappings
          _    <- unseenTypeMappings(seen).traverse_(tm => addProblem(UnusedTypeMapping(tm)))
          _    <- mappings.traverse_(refChecks)
        } yield ()

      res.runS(initialState).problems.reverse
    }
  }

  object TypeMappings {
    def apply(mappings: Seq[TypeMapping]): TypeMappings =
      new TypeMappings(mappings, false)

    def apply(mappings: TypeMapping*)(implicit dummy: DummyImplicit): TypeMappings =
      apply(mappings)

    def unsafe(mappings: Seq[TypeMapping]): TypeMappings =
      new TypeMappings(mappings, true)

    def unsafe(mappings: TypeMapping*)(implicit dummy: DummyImplicit): TypeMappings =
      unsafe(mappings)

    implicit def fromList(mappings: List[TypeMappingCompat]): TypeMappings = TypeMappings(mappings.flatMap(_.unwrap))

    val empty: TypeMappings = unsafe(Nil)

    private type MappingValidator[T] = StateT[Id, MappingValidator.State, T]
    private object MappingValidator {
      type MV[T] = MappingValidator[T]
      def unit: MV[Unit] = StateT.pure(())
      def pure[T](t: T): MV[T] = StateT.pure(t)
      def addProblem(p: ValidationFailure): MV[Unit] = StateT.modify(_.addProblem(p))
      def addProblems(ps: List[ValidationFailure]): MV[Unit] = StateT.modify(_.addProblems(ps))
      def seenType(tpe: Type): MV[Boolean] = StateT.inspect(_.seenType(tpe))
      def addSeenType(tpe: Type): MV[Unit] = StateT.modify(_.addSeenType(tpe))
      def addSeenTypeMapping(tm: TypeMapping): MV[Unit] = StateT.modify(_.addSeenTypeMapping(tm))
      def seenTypeMappings: MV[Set[TypeMapping]] = StateT.inspect(_.seenTypeMappings)
      def addSeenFieldMapping(om: ObjectMapping, fm: FieldMapping): MV[Unit] = StateT.modify(_.addSeenFieldMapping(om, fm))
      def seenFieldMappings(om: ObjectMapping): MV[Set[FieldMapping]] = StateT.inspect(_.seenFieldMappings(om))

      case class State(
        seenTypes: Set[String],
        seenTypeMappings: Set[TypeMapping],
        seenFieldMappings0: Map[ObjectMapping, Set[FieldMapping]],
        problems: List[ValidationFailure]
      ) {
        def addProblem(p: ValidationFailure): State =
          copy(problems = p :: problems)
        def addProblems(ps: List[ValidationFailure]): State =
          copy(problems = ps ::: problems)
        def seenType(tpe: Type): Boolean =
          tpe match {
            case nt: NamedType => seenTypes(nt.name)
            case _ => false
          }
        def addSeenType(tpe: Type): State =
          tpe match {
            case nt: NamedType => copy(seenTypes = seenTypes + nt.name)
            case _ => this
          }

        def addSeenTypeMapping(tm: TypeMapping): State =
          copy(seenTypeMappings = seenTypeMappings + tm)

        def addSeenFieldMapping(om: ObjectMapping, fm: FieldMapping): State =
          copy(seenTypeMappings = seenTypeMappings + om, seenFieldMappings0 = seenFieldMappings0.updatedWith(om)(_.map(_ + fm).orElse(Set(fm).some)))

        def seenFieldMappings(om: ObjectMapping): Set[FieldMapping] =
          seenFieldMappings0.getOrElse(om, Set.empty)
      }

      def initialState: State =
        State(Set.empty, Set.empty, Map.empty, Nil)
    }
  }


  /** Check Mapping specific TypeMapping validity */
  protected def validateTypeMapping(mappings: TypeMappings, context: Context, tm: TypeMapping): List[ValidationFailure] = Nil

  /** Check Mapping specific FieldMapping validity */
  protected def validateFieldMapping(mappings: TypeMappings, context: Context, om: ObjectMapping, fm: FieldMapping): List[ValidationFailure] = Nil

  /**
   * Validatate this Mapping, yielding a chain of `Failure`s of severity equal to or greater than the
   * specified `Severity`.
   */
  def validate(severity: Severity = Severity.Warning): List[ValidationFailure] = {
    typeMappings.validate.filter(_.severity >= severity)
  }

  /**
   * Run this validator, raising a `ValidationException` in `F` if there are any failures of
   * severity equal to or greater than the specified `Severity`.
   */
  def validateInto[G[_]](severity: Severity = Severity.Warning)(
    implicit ev: ApplicativeError[G, Throwable]
  ): G[Unit] =
    NonEmptyList.fromList(validate(severity)).foldMapA(nec => ev.raiseError(ValidationException(nec)))

  /**
   * Run this validator, raising a `ValidationException` if there are any failures of severity equal
   * to or greater than the specified `Severity`.
   */
  def unsafeValidate(severity: Severity = Severity.Warning): Unit =
    validateInto[Either[Throwable, *]](severity).fold(throw _, _ => ())

  /** Yields the `RootEffect`, if any, associated with `fieldName`. */
  def rootEffect(context: Context, fieldName: String): Option[RootEffect] =
    typeMappings.fieldMapping(context, fieldName).collect {
      case re: RootEffect => re
    }

  /** Yields the `RootStream`, if any, associated with `fieldName`. */
  def rootStream(context: Context, fieldName: String): Option[RootStream] =
    typeMappings.fieldMapping(context, fieldName).collect {
      case rs: RootStream => rs
    }

  /** Yields the `Encoder` associated with the provided leaf context, if any. */
  def encoderForLeaf(context: Context): Option[Encoder[Any]] =
    typeMappings.typeMapping(context).collect {
      case lm: LeafMapping[_] => lm.encoder.asInstanceOf[Encoder[Any]]
    }

  sealed trait TypeMappingCompat {
    private[grackle] def unwrap: Seq[TypeMapping] =
      this match {
        case TypeMappingCompat.PrefixedMappingCompat(mappings) => mappings
        case tm: TypeMapping => Seq(tm)
      }
  }

  object TypeMappingCompat {
    case class PrefixedMappingCompat(mappings0: Seq[TypeMapping]) extends TypeMappingCompat
  }

  protected def unpackPrefixedMapping(prefix: List[String], om: ObjectMapping): ObjectMapping =
    om match {
      case om: ObjectMapping.DefaultObjectMapping =>
        om.copy(predicate = MappingPredicate.PrefixedTypeMatch(prefix, om.predicate.tpe))
      case other => other
    }

  /** Backwards compatible constructor for legacy `PrefixedMapping` */
  def PrefixedMapping(tpe: NamedType, mappings: List[(List[String], ObjectMapping)]): TypeMappingCompat.PrefixedMappingCompat = {
    if (!mappings.forall(_._2.predicate.tpe =:= tpe))
      throw new IllegalArgumentException("All prefixed mappings must have the same type")

    TypeMappingCompat.PrefixedMappingCompat(
      mappings.map { case (prefix, om) => unpackPrefixedMapping(prefix, om) }
    )
  }

  sealed trait TypeMapping extends TypeMappingCompat with Product with Serializable {
    def predicate: MappingPredicate
    def pos: SourcePos

    def tpe: NamedType = predicate.tpe

    def showMappingType: String = productPrefix
  }

  /** A predicate determining the applicability of a `TypeMapping` in a given context */
  trait MappingPredicate {
    /** The type to which this predicate applies */
    def tpe: NamedType
    /**
     * Does this predicate apply to the given context?
     *
     * Yields the priority of the corresponding `TypeMapping` if the predicate applies,
     * or `None` otherwise.
     */
    def apply(ctx: Context): Option[Int]

    /**
     * Given a Context, yield a strictly extended Context which would be matched by this
     * predicate, if any, None otherwise.
     */
    def continuationContext(ctx: Context): Option[Context]
  }

  object MappingPredicate {
    /**
     * Extend the given context by the given path, if possible, navigating through interfaces
     * and unions as necessary.
     */
    def extendContext(ctx: Context, path: List[String]): Option[Context] =
      if(path.isEmpty) Some(ctx)
      else
        ctx.tpe.underlyingNamed.dealias match {
          case ot: ObjectType =>
            ot.fieldInfo(path.head) match {
              case Some(_) =>
                extendContext(ctx.forFieldOrAttribute(path.head, None), path.tail)
              case None =>
                None
            }
          case it: InterfaceType =>
            it.fieldInfo(path.head) match {
              case Some(_) =>
                extendContext(ctx.forFieldOrAttribute(path.head, None), path.tail)
              case None =>
                val implementors = schema.types.collect { case ot: ObjectType if ot <:< it => ot }
                implementors.collectFirstSome(tpe => extendContext(ctx.asType(tpe), path))
            }
          case ut: UnionType =>
            ut.members.collectFirstSome(tpe => extendContext(ctx.asType(tpe), path))

          case _ =>
            None
        }

    /** A predicate that matches a specific type in any context */
    case class TypeMatch(tpe: NamedType) extends MappingPredicate {
      def apply(ctx: Context): Option[Int] =
        if (ctx.tpe.underlyingNamed =:= tpe)
          Some(0)
        else
          None

      /**
       * Given a Context, yield a strictly extended Context which would be matched by this
       * predicate, if any, None otherwise.
       *
       * For a TypeMatch predicate, there is no continuation context.
       */
      def continuationContext(ctx: Context): Option[Context] = None
    }

    /**
     * A predicate that matches a specific type with a given path prefix.
     *
     * This predicate corresponds to the semantics of the `PrefixedMapping` in earlier
     * releases.
     */
    case class PrefixedTypeMatch(prefix: List[String], tpe: NamedType) extends MappingPredicate {
      def apply(ctx: Context): Option[Int] =
        if (
          ctx.tpe.underlyingNamed =:= tpe &&
          ctx.path.startsWith(prefix.reverse)
        )
          Some(prefix.length)
        else
          None

      /**
       * Given a Context, yield a strictly extended Context which would be matched by this
       * predicate, if any, None otherwise.
       *
       * For a PrefixedTypeMatch predicate, the contination context is the given context
       * extended by the prefix path, navigating through interfaces and unions as necessary.
       */
      def continuationContext(ctx: Context): Option[Context] =
        extendContext(ctx, prefix)
    }

    /**
     * A predicate that matches the given `Path` as the suffix of the context path.
     *
     * Note that a `Path` corresponds to an initial type, followed by a sequence of
     * field selectors. The type found by following the field selectors from the initial
     * type determines the final type to which the predicate applies.
     *
     * This predicate is thus a slightly more restrictive variant of `PrefixedTypeMatch`
     * which will match in any context with the given path and final type, irrespective
     * of the initial type.
     *
     * In practice `PathMatch` is more convenient to use in most
     * circumstances and should be preferred to `PrefixedTypeMatch` unless the semantics
     * of the latter are absolutely required.
     */
    case class PathMatch(path: Path) extends MappingPredicate {
      lazy val tpe: NamedType = path.tpe.underlyingNamed

      def apply(ctx: Context): Option[Int] =
        if (
          ctx.tpe.underlyingNamed =:= tpe &&
          ctx.path.startsWith(path.path.reverse) &&
          ((ctx.path.lengthCompare(path.path) == 0 && ctx.rootTpe.underlyingNamed =:= path.rootTpe.underlyingNamed) ||
           ctx.typePath.drop(path.path.length).headOption.exists(_.underlyingNamed =:= path.rootTpe.underlyingNamed))
        )
          Some(if(path.path.isEmpty) 0 else path.path.length+1)
        else
          None

      /**
       * Given a Context, yield a strictly extended Context which would be matched by this
       * predicate, if any, None otherwise.
       *
       * For a PathMatch predicate, the contination context is the given context
       * extended by the prefix path, navigating through interfaces and unions as necessary,
       * but only if the path root type matches the given context type.
       */
      def continuationContext(ctx: Context): Option[Context] =
        if(path.rootTpe.underlyingNamed =:= ctx.tpe.underlyingNamed) extendContext(ctx, path.path)
        else None
    }
  }

  abstract class ObjectMapping extends TypeMapping {
    private lazy val fieldMappingIndex = fieldMappings.map(fm => (fm.fieldName, fm)).toMap

    def fieldMappings: Seq[FieldMapping]
    def fieldMapping(fieldName: String): Option[FieldMapping] = fieldMappingIndex.get(fieldName)
  }

  object ObjectMapping {
    case class DefaultObjectMapping(predicate: MappingPredicate, fieldMappings: Seq[FieldMapping])(
      implicit val pos: SourcePos
    ) extends ObjectMapping {
      override def showMappingType: String = "ObjectMapping"
    }

    def apply(predicate: MappingPredicate)(fieldMappings: FieldMapping*)(
      implicit pos: SourcePos
    ): ObjectMapping =
      DefaultObjectMapping(predicate, fieldMappings)

    def apply(tpe: NamedType)(fieldMappings: FieldMapping*)(
      implicit pos: SourcePos
    ): ObjectMapping =
      DefaultObjectMapping(MappingPredicate.TypeMatch(tpe), fieldMappings)

    def apply(path: Path)(fieldMappings: FieldMapping*)(
      implicit pos: SourcePos
    ): ObjectMapping =
      DefaultObjectMapping(MappingPredicate.PathMatch(path), fieldMappings)

    def apply(tpe: NamedType, fieldMappings: List[FieldMapping])(
      implicit pos: SourcePos
    ): ObjectMapping =
      DefaultObjectMapping(MappingPredicate.TypeMatch(tpe), fieldMappings)
  }

  trait FieldMapping extends Product with Serializable {
    def fieldName: String
    def hidden: Boolean
    def subtree: Boolean
    def pos: SourcePos

    def showMappingType: String = productPrefix
  }

  /**
    * Abstract type of field mappings with effects.
    */
  trait EffectMapping extends FieldMapping {
    def subtree: Boolean = true
  }

  case class EffectField(fieldName: String, handler: EffectHandler[F], required: List[String] = Nil, hidden: Boolean = false)(implicit val pos: SourcePos)
    extends EffectMapping

  /**
   * Root effects can perform an intial effect prior to computing the resulting
   * `Cursor` and effective `Query`.
   *
   * These effects are used to perform initial effectful setup for a query or to
   * perform the effect associated with a GraphQL mutation. Convenience methods
   * are provided to cover the cases where only one of the query or the cursor
   * are computed.
   *
   * If only the query is computed the default root cursor for the mapping will
   * be used. If only the cursor is computed the client query (after elaboration)
   * is used unmodified ... in this case results of the performed effect can only
   * be passed to the result construction stage via the environment associated
   * with the returned cursor.
   */
  case class RootEffect private (fieldName: String, effect: (Query, Path, Env) => F[Result[(Query, Cursor)]])(implicit val pos: SourcePos)
    extends EffectMapping {
    def hidden = false
    def toRootStream: RootStream = RootStream(fieldName)((q, p, e) => Stream.eval(effect(q, p, e)))
  }

  object RootEffect {
    /**
     * Yields a `RootEffect` which performs both an initial effect and yields an effect-specific query and
     * corresponding root cursor.
     */
    def apply(fieldName: String)(effect: (Query, Path, Env) => F[Result[(Query, Cursor)]])(implicit pos: SourcePos, di: DummyImplicit): RootEffect =
      new RootEffect(fieldName, effect)

    /**
     * Yields a `RootEffect` which performs an initial effect which leaves the query and default root cursor
     * unchanged.
     */
    def computeUnit(fieldName: String)(effect: Env => F[Result[Unit]])(implicit pos: SourcePos): RootEffect =
      new RootEffect(
        fieldName,
        (query, path, env) =>
          (for {
            _  <- ResultT(effect(env))
            qc <- ResultT(defaultRootCursor(query, path.rootTpe, None))
          } yield qc.map(_.withEnv(env))).value
      )

    /**
      * Yields a `RootEffect` which performs an initial effect and yields an effect-specific root cursor.
      */
    def computeCursor(fieldName: String)(effect: (Path, Env) => F[Result[Cursor]])(implicit pos: SourcePos): RootEffect =
      new RootEffect(
        fieldName,
        (query, path, env) => effect(path, env).map(_.map(c => (query, c)))
      )

    /**
      * Yields a `RootEffect` which performs an initial effect and yields an effect-specific query
      * which is executed with respect to the default root cursor for the corresponding `Mapping`.
      */
    def computeChild(fieldName: String)(effect: (Query, Path, Env) => F[Result[Query]])(implicit pos: SourcePos): RootEffect =
      new RootEffect(
        fieldName,
        (query, path, env) =>
          (for {
            child <- ResultT(Query.extractChild(query).toResultOrError("Root query has unexpected shape").pure[F])
            q     <- ResultT(effect(child, path, env).map(_.flatMap(Query.substChild(query, _).toResultOrError("Root query has unexpected shape"))))
            qc    <- ResultT(defaultRootCursor(q, path.rootTpe, None))
          } yield qc.map(_.withEnv(env))).value
      )
  }

  /**
   * Root streams can perform an intial effect prior to emitting the resulting
   * cursors and effective queries.
   *
   * Stream effects are used for GraphQL subscriptions. Convenience methods are
   * provided to cover the cases where only one of the query or the cursor are
   * computed
   *
   * If only the query is computed the default root cursor for the mapping will
   * be used. If only the cursor is computed the client query (after elaboration)
   * is used unmodified ... in this case results of the performed effect can only
   * be passed to the result construction stage via the environment associated
   * with the returned cursor.
   */
  case class RootStream private (fieldName: String, effect: (Query, Path, Env) => Stream[F, Result[(Query, Cursor)]])(implicit val pos: SourcePos)
    extends EffectMapping {
    def hidden = false
  }

  object RootStream {
    /**
     * Yields a `RootStream` which performs both an initial effect and yields an effect-specific query and
     * corresponding root cursor.
     */
    def apply(fieldName: String)(effect: (Query, Path, Env) => Stream[F, Result[(Query, Cursor)]])(implicit pos: SourcePos, di: DummyImplicit): RootStream =
      new RootStream(fieldName, effect)

    /**
      * Yields a `RootStream` which yields a stream of effect-specific root cursors.
      *
      * This form of effect is typically used to implement GraphQL subscriptions.
      */
    def computeCursor(fieldName: String)(effect: (Path, Env) => Stream[F, Result[Cursor]])(implicit pos: SourcePos): RootStream =
      new RootStream(
        fieldName,
        (query, path, env) => effect(path, env).map(_.map(c => (query, c)))
      )

    /**
      * Yields a `RootStream` which yields a stream of effect-specific queries
      * which are executed with respect to the default root cursor for the
      * corresponding `Mapping`.
      *
      * This form of effect is typically used to implement GraphQL subscriptions.
      */
    def computeChild(fieldName: String)(effect: (Query, Path, Env) => Stream[F, Result[Query]])(implicit pos: SourcePos): RootStream =
      new RootStream(
        fieldName,
        (query, path, env) =>
          Query.extractChild(query).fold(Stream.emit[F, Result[(Query, Cursor)]](Result.internalError("Root query has unexpected shape"))) { child =>
            effect(child, path, env).flatMap(child0 =>
              Stream.eval(
                (for {
                  q  <- ResultT(child0.flatMap(Query.substChild(query, _).toResultOrError("Root query has unexpected shape")).pure[F])
                  qc <- ResultT(defaultRootCursor(q, path.rootTpe, None))
                } yield qc.map(_.withEnv(env))).value
              )
            )
          }
      )
  }

  trait LeafMapping[T] extends TypeMapping {
    def encoder: Encoder[T]
    def scalaTypeName: String
    def pos: SourcePos
  }

  object LeafMapping {

    case class DefaultLeafMapping[T](predicate: MappingPredicate, encoder: Encoder[T], scalaTypeName: String)(
      implicit val pos: SourcePos
    ) extends LeafMapping[T] {
      override def showMappingType: String = "LeafMapping"
    }

    def apply[T: TypeName](predicate: MappingPredicate)(implicit encoder: Encoder[T], pos: SourcePos): LeafMapping[T] =
      DefaultLeafMapping(predicate, encoder, typeName)

    def apply[T: TypeName](tpe: NamedType)(implicit encoder: Encoder[T], pos: SourcePos): LeafMapping[T] =
      DefaultLeafMapping(MappingPredicate.TypeMatch(tpe), encoder, typeName)

    def apply[T: TypeName](path: Path)(implicit encoder: Encoder[T], pos: SourcePos): LeafMapping[T] =
      DefaultLeafMapping(MappingPredicate.PathMatch(path), encoder, typeName)

    def unapply[T](lm: LeafMapping[T]): Option[(Type, Encoder[T])] =
      Some((lm.tpe, lm.encoder))
  }

  case class CursorField[T](fieldName: String, f: Cursor => Result[T], encoder: Encoder[T], required: List[String], hidden: Boolean)(
    implicit val pos: SourcePos
  ) extends FieldMapping {
    def subtree = false
  }
  object CursorField {
    def apply[T](fieldName: String, f: Cursor => Result[T], required: List[String] = Nil, hidden: Boolean = false)(implicit encoder: Encoder[T], di: DummyImplicit): CursorField[T] =
      new CursorField(fieldName, f, encoder, required, hidden)
  }

  case class Delegate(
    fieldName: String,
    mapping: Mapping[F],
    join: (Query, Cursor) => Result[Query] = ComponentElaborator.TrivialJoin
  )(implicit val pos: SourcePos) extends FieldMapping {
    def hidden = false
    def subtree = true
  }

  val selectElaborator: SelectElaborator = SelectElaborator.identity

  lazy val componentElaborator = {
    val componentMappings =
      typeMappings.mappings.flatMap {
        case om: ObjectMapping =>
          om.fieldMappings.collect {
            case Delegate(fieldName, mapping, join) =>
              ComponentElaborator.ComponentMapping(schema.uncheckedRef(om.tpe), fieldName, mapping, join)
          }
        case _ => Nil
      }

    ComponentElaborator(componentMappings)
  }

  lazy val effectElaborator = {
    val effectMappings =
      typeMappings.mappings.flatMap {
        case om: ObjectMapping =>
          om.fieldMappings.collect {
            case EffectField(fieldName, handler, _, _) =>
              EffectElaborator.EffectMapping(schema.uncheckedRef(om.tpe), fieldName, handler)
          }
        case _ => Nil
      }

    EffectElaborator(effectMappings)
  }

  def compilerPhases: List[QueryCompiler.Phase] = List(selectElaborator, componentElaborator, effectElaborator)

  def parserConfig: GraphQLParser.Config = GraphQLParser.defaultConfig
  lazy val graphQLParser: GraphQLParser = GraphQLParser(parserConfig)
  lazy val queryParser: QueryParser = QueryParser(graphQLParser)

  private def deferredValidate(): Unit = {
    if(!typeMappings.unsafe)
      unsafeValidate()
  }

  lazy val compiler: QueryCompiler = {
    deferredValidate()
    new QueryCompiler(queryParser, schema, compilerPhases)
  }

  val interpreter: QueryInterpreter[F] = new QueryInterpreter(this)

  /** Cursor positioned at a GraphQL result leaf */
  case class LeafCursor(context: Context, focus: Any, parent: Option[Cursor], env: Env) extends Cursor {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def mkChild(context: Context = context, focus: Any = focus): LeafCursor =
      LeafCursor(context, focus, Some(this), Env.empty)

    def isLeaf: Boolean = tpe.isLeaf

    def asLeaf: Result[Json] =
      encoderForLeaf(context).map(enc => enc(focus).success).getOrElse(Result.internalError(
        s"Cannot encode value $focus at ${context.path.reverse.mkString("/")} (of GraphQL type ${context.tpe}). Did you forget a LeafMapping?".stripMargin.trim
      ))

    def preunique: Result[Cursor] = {
      val listTpe = tpe.nonNull.list
      focus match {
        case _: List[_] => mkChild(context.asType(listTpe), focus).success
        case _ =>
          Result.internalError(s"Expected List type, found $focus for ${listTpe}")
      }
    }

    def isList: Boolean =
      tpe match {
        case ListType(_) => true
        case _ => false
      }

    def asList[C](factory: Factory[Cursor, C]): Result[C] = (tpe, focus) match {
      case (ListType(tpe), it: Seq[_]) => it.view.map(f => mkChild(context.asType(tpe), focus = f)).to(factory).success
      case _ => Result.internalError(s"Expected List type, found $tpe")
    }

    def listSize: Result[Int] = (tpe, focus) match {
      case (ListType(_), it: Seq[_]) => it.size.success
      case _ => Result.internalError(s"Expected List type, found $tpe")
    }

    def isNullable: Boolean =
      tpe match {
        case NullableType(_) => true
        case _ => false
      }

    def asNullable: Result[Option[Cursor]] =
      (tpe, focus) match {
        case (NullableType(_), None) => None.success
        case (NullableType(tpe), Some(v)) => Some(mkChild(context.asType(tpe), focus = v)).success
        case _ => Result.internalError(s"Not nullable at ${context.path}")
      }

    def isDefined: Result[Boolean] =
      (tpe, focus) match {
        case (NullableType(_), opt: Option[_]) => opt.isDefined.success
        case _ => Result.internalError(s"Not nullable at ${context.path}")
      }

    def narrowsTo(subtpe: TypeRef): Boolean = false
    def narrow(subtpe: TypeRef): Result[Cursor] =
      Result.failure(s"Cannot narrow $tpe to $subtpe")

    def hasField(fieldName: String): Boolean = false
    def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      Result.failure(s"Cannot select field '$fieldName' from leaf type $tpe")
  }

  /**
   * Proxy `Cursor` which applies a function to the focus of an underlying `LeafCursor`.
   */
  case class FieldTransformCursor[T : ClassTag : TypeName](underlying: Cursor, f: T => Result[T]) extends ProxyCursor(underlying) {
    override def withEnv(env: Env): Cursor = new FieldTransformCursor(underlying.withEnv(env), f)
    override def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      underlying.field(fieldName, resultName).flatMap {
        case l: LeafCursor =>
          for {
            focus  <- l.as[T]
            ffocus <- f(focus)
          } yield l.copy(focus = ffocus)
        case _ =>
          Result.internalError(s"Expected leaf cursor for field $fieldName")
      }
  }

  /**
   * Construct a GraphQL response from the possibly absent result `data`
   * and a collection of errors.
   */
  def mkResponse(data: Option[Json], errors: Chain[Problem]): Json = {
    val dataField = data.map { value => ("data", value) }.toList
    val fields =
      (dataField, errors.toList) match {
        case (Nil, Nil)   => List(("errors", Json.fromValues(List(Problem("Invalid query").asJson))))
        case (data, Nil)  => data
        case (data, errs) => ("errors", errs.asJson) :: data
      }
    Json.fromFields(fields)
  }

  /** Construct a GraphQL response from a `Result`. */
  def mkResponse(result: Result[Json]): F[Json] =
    result match {
      case Result.InternalError(err) => M.raiseError(err)
      case _ => mkResponse(result.toOption, result.toProblems).pure[F]
    }

  /** Missing type mapping. */
  case class MissingTypeMapping(ctx: Context)
    extends ValidationFailure(Severity.Error) {
    override def toString: String =
      s"$productPrefix(${showNamedType(ctx.tpe)})"
    override def formattedMessage: String =
      s"""|Missing type mapping.
          |
          |- The type ${graphql(showNamedType(ctx.tpe))} is defined by a Schema at (1).
          |- ${UNDERLINED}No mapping was found for this type for path ${ctx.path.reverse.mkString("", "/", "")}.$RESET
          |
          |(1) ${schema.pos}
          |""".stripMargin
  }

  /** Ambiguous type mappings. */
  case class AmbiguousTypeMappings(ctx: Context, conflicts: Seq[TypeMapping])
    extends ValidationFailure(Severity.Error) {
    override def toString: String =
      s"$productPrefix(${showNamedType(ctx.tpe)})"
    override def formattedMessage: String = {
      val n = conflicts.length
      val ref = if(n > 2) s"(2)..(${n+1})" else "(2), (3)"
      val posns = conflicts.zip(2 to n+1).map { case (c, n) => s"($n) ${c.pos}" }.mkString("\n")
      s"""|Ambiguous type mappings.
          |
          |- The type ${graphql(showNamedType(ctx.tpe))} is defined by a Schema at (1).
          |- Multiple equally specific mappings were found at $ref for this type for path ${ctx.path.reverse.mkString("", "/", "")}.
          |- ${UNDERLINED}Mappings must be unambiguous.$RESET
          |
          |(1) ${schema.pos}
          |$posns
          |""".stripMargin
    }
  }

  /** Object type `owner` declares `field` but no such mapping exists. */
  case class MissingFieldMapping(objectMapping: ObjectMapping, field: Field)
    extends ValidationFailure(Severity.Error) {
    override def toString: String =
      s"$productPrefix(${showNamedType(objectMapping.tpe)}.${field.name}:${showType(field.tpe)})"
    override def formattedMessage: String =
      s"""|Missing field mapping.
          |
          |- The field ${graphql(s"${showNamedType(objectMapping.tpe)}.${field.name}:${showType(field.tpe)}")} is defined by a Schema at (1).
          |- The ${scala(objectMapping.showMappingType)} for ${graphql(showNamedType(objectMapping.tpe))} at (2) ${UNDERLINED}does not define a mapping for this field$RESET.
          |
          |(1) ${schema.pos}
          |(2) ${objectMapping.pos}
          |""".stripMargin
  }

  /** GraphQL type isn't applicable for mapping type. */
  case class ObjectTypeExpected(objectMapping: ObjectMapping)
    extends ValidationFailure(Severity.Error) {
    override def toString: String =
      s"$productPrefix(${objectMapping.showMappingType}, ${showNamedType(objectMapping.tpe)})"
    override def formattedMessage: String =
      s"""|Inapplicable GraphQL type.
          |
          |- The ${typeKind(objectMapping.tpe)} ${graphql(showNamedType(objectMapping.tpe))} is defined by a Schema at (1).
          |- It is mapped by the ${scala(objectMapping.showMappingType)} at (2), which expects an object type.
          |- ${UNDERLINED}Use a different kind of mapping for this type.$RESET
          |
          |(1) ${schema.pos}
          |(2) ${objectMapping.pos}
          |""".stripMargin
  }

  /** GraphQL type isn't applicable for mapping type. */
  case class LeafTypeExpected(leafMapping: LeafMapping[_])
    extends ValidationFailure(Severity.Error) {
    override def toString: String =
      s"$productPrefix(${leafMapping.showMappingType}, ${showNamedType(leafMapping.tpe)})"
    override def formattedMessage: String =
      s"""|Inapplicable GraphQL type.
          |
          |- The ${typeKind(leafMapping.tpe)} ${graphql(showNamedType(leafMapping.tpe))} is defined by a Schema at (1).
          |- It is mapped by the ${scala(leafMapping.showMappingType)} at (2), which expects a leaf type.
          |- ${UNDERLINED}Use a different kind of mapping for this type.$RESET
          |
          |(1) ${schema.pos}
          |(2) ${leafMapping.pos}
          |""".stripMargin
  }


  /** Referenced type does not exist. */
  case class ReferencedTypeDoesNotExist(typeMapping: TypeMapping)
    extends ValidationFailure(Severity.Error) {
    override def toString: String =
      s"$productPrefix(${typeMapping.showMappingType}, ${showNamedType(typeMapping.tpe)})"
    override def formattedMessage: String =
      s"""|Referenced type does not exist.
          |
          |- The ${scala(typeMapping.showMappingType)} at (1) references type ${graphql(showNamedType(typeMapping.tpe))}.
          |- ${UNDERLINED}This type is undeclared$RESET in the Schema defined at (2).
          |
          |(1) ${typeMapping.pos}
          |(2) ${schema.pos}
          |""".stripMargin
  }

  /** Type mapping is unused. */
  case class UnusedTypeMapping(typeMapping: TypeMapping)
    extends ValidationFailure(Severity.Warning) {
    override def toString: String =
      s"$productPrefix(${typeMapping.showMappingType}, ${showNamedType(typeMapping.tpe)})"
    override def formattedMessage: String =
      s"""|Type mapping is unused.
          |
          |- The ${scala(typeMapping.showMappingType)} at (1) references type ${graphql(showNamedType(typeMapping.tpe))}.
          |- ${UNDERLINED}This type mapping is unused$RESET by queries conforming to the Schema at (2).
          |
          |(1) ${typeMapping.pos}
          |(2) ${schema.pos}
          |""".stripMargin
  }

  /** Referenced field does not exist. */
  case class UnusedFieldMapping(objectMapping: ObjectMapping, fieldMapping: FieldMapping)
    extends ValidationFailure(Severity.Error) {
    override def toString: String =
      s"$productPrefix(${showNamedType(objectMapping.tpe)}.${fieldMapping.fieldName})"
    override def formattedMessage: String =
      s"""|Field mapping is unused.
          |
          |- The ${scala(objectMapping.showMappingType)} at (1) contains a ${scala(fieldMapping.showMappingType)} mapping for field ${graphql(fieldMapping.fieldName)} at (2).
          |- ${UNDERLINED}This field mapping is unused$RESET by queries conforming to the Schema at (3).
          |
          |(1) ${objectMapping.pos}
          |(2) ${fieldMapping.pos}
          |(3) ${schema.pos}
          |""".stripMargin
  }
}
