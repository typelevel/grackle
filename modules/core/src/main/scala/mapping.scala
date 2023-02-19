// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import scala.collection.Factory

import cats.Monad
import cats.data.{Chain, Ior, IorT}
import cats.implicits._
import fs2.{ Stream, Compiler }
import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.tpolecat.sourcepos.SourcePos
import org.tpolecat.typename._

import Cursor.{AbstractCursor, Context, Env}
import Query.Select
import QueryCompiler.{ComponentElaborator, SelectElaborator, IntrospectionLevel}
import QueryInterpreter.{mkErrorResult, ProtoJson}
import IntrospectionLevel._

trait QueryExecutor[F[_], T] { outer =>
  def compileAndRun(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, env: Env = Env.empty)(
    implicit sc: Compiler[F,F]
  ): F[T] =
    compileAndRunOne(text, name, untypedVars, introspectionLevel, env)

  def compileAndRunAll(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, env: Env = Env.empty): Stream[F,T]

  def compileAndRunOne(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, env: Env = Env.empty)(
    implicit sc: Compiler[F,F]
  ): F[T]
}

abstract class Mapping[F[_]] extends QueryExecutor[F, Json] {
  implicit val M: Monad[F]
  val schema: Schema
  val typeMappings: List[TypeMapping]

  def run(query: Query, rootTpe: Type, env: Env): Stream[F,Json] =
    interpreter.run(query, rootTpe, env)

  def run(op: Operation, env: Env = Env.empty): Stream[F,Json] =
    run(op.query, op.rootTpe, env)

  def compileAndRunOne(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, env: Env = Env.empty)(
    implicit sc: Compiler[F,F]
  ): F[Json] =
    compileAndRunAll(text, name, untypedVars, introspectionLevel, env).compile.toList.map {
      case List(j) => j
      case Nil     => QueryInterpreter.mkError("Result stream was empty.").asJson
      case js      => QueryInterpreter.mkError(s"Result stream contained ${js.length} results; expected exactly one.").asJson
    }

  def compileAndRunAll(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, env: Env = Env.empty): Stream[F,Json] =
    compiler.compile(text, name, untypedVars, introspectionLevel) match {
      case Ior.Right(operation) =>
        run(operation.query, operation.rootTpe, env)
      case invalid =>
        QueryInterpreter.mkInvalidResponse(invalid).pure[Stream[F,*]]
    }

  def combineQueries
    (queries: List[(Query, Type, Env)])
    (exec: List[(Query, Type, Env)] => F[(Chain[Problem], List[ProtoJson])]): F[(Chain[Problem], List[ProtoJson])] = {
    exec(queries)
  }

  /** Yields a `Cursor` focused on the top level operation type of the query */
  def defaultRootCursor(query: Query, tpe: Type, env: Env): F[Result[(Query, Cursor)]] =
    Result((query, RootCursor(Context(tpe), None, env))).pure[F].widen

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
      fieldMapping(context, fieldName).isDefined

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
      LeafCursor(fieldContext, focus, Some(parent), parent.env).rightIor

    fieldMapping(context, fieldName) match {
      case Some(_ : EffectMapping) =>
        mkLeafCursor(parent.focus)
      case Some(CursorField(_, f, _, _, _)) =>
        f(parent).flatMap(res => mkLeafCursor(res))
      case _ =>
        mkErrorResult(s"No field '$fieldName' for type ${parent.tpe}")
    }
  }

  def typeMapping(tpe: NamedType): Option[TypeMapping] =
    typeMappingIndex.get(tpe.name)

  private lazy val typeMappingIndex =
    typeMappings.flatMap(tm => tm.tpe.asNamed.map(tpe => (tpe.name, tm)).toList).toMap

  val validator: MappingValidator =
    MappingValidator(this)

  def objectMapping(context: Context): Option[ObjectMapping] =
    context.tpe.underlyingObject.flatMap { obj =>
      obj.asNamed.flatMap(typeMapping) match {
        case Some(om: ObjectMapping) => Some(om)
        case Some(pm: PrefixedMapping) =>
          val revPath = context.path.reverse
          pm.mappings.filter(m => revPath.endsWith(m._1)).maxByOption(_._1.length).map(_._2)
        case _ => None
      }
    }

  def fieldMapping(context: Context, fieldName: String): Option[FieldMapping] =
    objectMapping(context).flatMap(_.fieldMapping(fieldName)).orElse {
      context.tpe.underlyingObject match {
        case Some(ot: ObjectType) =>
          ot.interfaces.collectFirstSome(nt => fieldMapping(context.asType(nt), fieldName))
        case _ => None
      }
    }

  /** Yields the `EffectMapping`, if any, associated with `fieldName`. */
  def effectMapping(context: Context, fieldName: String): Option[EffectMapping] =
    fieldMapping(context, fieldName).collect {
      case em: EffectMapping => em
    }

  /** Yields the `RootEffect`, if any, associated with `fieldName`. */
  def rootEffect(context: Context, fieldName: String): Option[RootEffect] =
    fieldMapping(context, fieldName).collect {
      case re: RootEffect => re
    }

  /** Yields the `RootStream`, if any, associated with `fieldName`. */
  def rootStream(context: Context, fieldName: String): Option[RootStream] =
    fieldMapping(context, fieldName).collect {
      case rs: RootStream => rs
    }

  def leafMapping[T](tpe: Type): Option[LeafMapping[T]] =
    typeMappings.collectFirst {
      case lm@LeafMapping(tpe0, _) if tpe0 =:= tpe => lm.asInstanceOf[LeafMapping[T]]
    }

  /**
   * True if the supplied type is a leaf with respect to the GraphQL schema
   * or mapping, false otherwise.
   */
  def isLeaf(tpe: Type): Boolean = tpe.underlying match {
    case (_: ScalarType)|(_: EnumType) => true
    case tpe => leafMapping(tpe).isDefined
  }

  def encoderForLeaf(tpe: Type): Option[Encoder[Any]] =
    encoderMemo.get(tpe.dealias)

  private lazy val encoderMemo: scala.collection.immutable.Map[Type, Encoder[Any]] = {
    val intTypeEncoder: Encoder[Any] =
      new Encoder[Any] {
        def apply(i: Any): Json = i match {
          case i: Int => Json.fromInt(i)
          case l: Long => Json.fromLong(l)
          case other => sys.error(s"Not an Int: $other")
        }
      }

    val floatTypeEncoder: Encoder[Any] =
      new Encoder[Any] {
        def apply(f: Any): Json = f match {
          case f: Float => Json.fromFloat(f).getOrElse(sys.error(s"Unrepresentable float $f"))
          case d: Double => Json.fromDouble(d).getOrElse(sys.error(s"Unrepresentable double $d"))
          case d: BigDecimal => Json.fromBigDecimal(d)
          case other => sys.error(s"Not a Float: $other")
        }
      }

    val definedEncoders: List[(Type, Encoder[Any])] =
      typeMappings.collect { case lm: LeafMapping[_] => (lm.tpe.dealias -> lm.encoder.asInstanceOf[Encoder[Any]]) }

    val defaultEncoders: List[(Type, Encoder[Any])] =
      List(
        ScalarType.StringType -> Encoder[String].asInstanceOf[Encoder[Any]],
        ScalarType.IntType -> intTypeEncoder,
        ScalarType.FloatType -> floatTypeEncoder,
        ScalarType.BooleanType -> Encoder[Boolean].asInstanceOf[Encoder[Any]],
        ScalarType.IDType -> Encoder[String].asInstanceOf[Encoder[Any]]
      )

    (definedEncoders ++ defaultEncoders).toMap
  }

  trait TypeMapping extends Product with Serializable {
    def tpe: Type
    def pos: SourcePos
  }

  case class PrimitiveMapping(tpe: Type)(implicit val pos: SourcePos) extends TypeMapping

  abstract class ObjectMapping extends TypeMapping {
    private lazy val fieldMappingIndex = fieldMappings.map(fm => (fm.fieldName, fm)).toMap

    def fieldMappings: List[FieldMapping]
    def fieldMapping(fieldName: String): Option[FieldMapping] = fieldMappingIndex.get(fieldName)
  }

  object ObjectMapping {

    case class DefaultObjectMapping(tpe: Type, fieldMappings: List[FieldMapping])(
      implicit val pos: SourcePos
    ) extends ObjectMapping

    def apply(tpe: Type, fieldMappings: List[FieldMapping])(
      implicit pos: SourcePos
    ): ObjectMapping =
      DefaultObjectMapping(tpe, fieldMappings.map(_.withParent(tpe)))
  }

  case class PrefixedMapping(tpe: Type, mappings: List[(List[String], ObjectMapping)])(
    implicit val pos: SourcePos
  ) extends TypeMapping

  trait FieldMapping extends Product with Serializable {
    def fieldName: String
    def hidden: Boolean
    def withParent(tpe: Type): FieldMapping
    def pos: SourcePos
  }

  case class PrimitiveField(fieldName: String, hidden: Boolean = false)(implicit val pos: SourcePos) extends FieldMapping {
    def withParent(tpe: Type): PrimitiveField = this
  }

  /**
    * Abstract type of field mappings with effects.
    */
  abstract class EffectMapping extends FieldMapping {
    def toRootStream: RootStream
  }

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
    def withParent(tpe: Type): RootEffect = this
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
      * Yields a `RootEffect` which performs an initial effect and yields an effect-specific root cursor.
      */
    def computeCursor(fieldName: String)(effect: (Query, Path, Env) => F[Result[Cursor]])(implicit pos: SourcePos): RootEffect =
      new RootEffect(
        fieldName,
        (query, path, env) => effect(query, path, env).map(_.map(c => (query, c)))
      )

    /**
      * Yields a `RootEffect` which performs an initial effect and yields an effect-specific query
      * which is executed with respect to the default root cursor for the corresponding `Mapping`.
      */
    def computeQuery(fieldName: String)(effect: (Query, Path, Env) => F[Result[Query]])(implicit pos: SourcePos): RootEffect =
      new RootEffect(
        fieldName,
        (query, path, env) =>
          (for {
            q  <- IorT(effect(query, path, env))
            qc <- IorT(defaultRootCursor(q, path.rootTpe, env))
          } yield qc).value
      )
  }

  /**
   * Root streams can perform an intial effect prior to emitting the resulting
   * cursors and effective queryies.
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
    def withParent(tpe: Type): RootStream = this
    def toRootStream: RootStream = this
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
    def computeCursor(fieldName: String)(effect: (Query, Path, Env) => Stream[F, Result[Cursor]])(implicit pos: SourcePos): RootStream =
      new RootStream(
        fieldName,
        (query, path, env) => effect(query, path, env).map(_.map(c => (query, c)))
      )

    /**
      * Yields a `RootStream` which yields a stream of effect-specific queries
      * which are executed with respect to the default root cursor for the
      * corresponding `Mapping`.
      *
      * This form of effect is typically used to implement GraphQL subscriptions.
      */
    def computeQuery(fieldName: String)(effect: (Query, Path, Env) => Stream[F, Result[Query]])(implicit pos: SourcePos): RootStream =
      new RootStream(
        fieldName,
        (query, path, env) =>
          effect(query, path, env).flatMap(rq =>
            Stream.eval(
              (for {
                q  <- IorT(rq.pure[F])
                qc <- IorT(defaultRootCursor(q, path.rootTpe, env))
              } yield qc).value
            )
          )
      )
  }

  trait LeafMapping[T] extends TypeMapping {
    def tpe: Type
    def encoder: Encoder[T]
    def scalaTypeName: String
    def pos: SourcePos
  }
  object LeafMapping {

    case class DefaultLeafMapping[T](tpe: Type, encoder: Encoder[T], scalaTypeName: String)(
      implicit val pos: SourcePos
    ) extends LeafMapping[T]

    def apply[T: TypeName](tpe: Type)(implicit encoder: Encoder[T], pos: SourcePos): LeafMapping[T] =
      DefaultLeafMapping(tpe, encoder, typeName)

    def unapply[T](lm: LeafMapping[T]): Option[(Type, Encoder[T])] =
      Some((lm.tpe, lm.encoder))
  }

  case class CursorField[T](fieldName: String, f: Cursor => Result[T], encoder: Encoder[T], required: List[String], hidden: Boolean)(
    implicit val pos: SourcePos
  ) extends FieldMapping {
    def withParent(tpe: Type): CursorField[T] = this
  }
  object CursorField {
    def apply[T](fieldName: String, f: Cursor => Result[T], required: List[String] = Nil, hidden: Boolean = false)(implicit encoder: Encoder[T], di: DummyImplicit): CursorField[T] =
      new CursorField(fieldName, f, encoder, required, hidden)
  }

  case class Delegate(
    fieldName: String,
    interpreter: Mapping[F],
    join: (Cursor, Query) => Result[Query] = ComponentElaborator.TrivialJoin
  )(implicit val pos: SourcePos) extends FieldMapping {
    def hidden = false
    def withParent(tpe: Type): Delegate = this
  }

  val selectElaborator: SelectElaborator = new SelectElaborator(Map.empty[TypeRef, PartialFunction[Select, Result[Query]]])

  lazy val componentElaborator = {
    val componentMappings =
      typeMappings.flatMap {
        case om: ObjectMapping =>
          om.fieldMappings.collect {
            case Delegate(fieldName, mapping, join) =>
              ComponentElaborator.ComponentMapping(schema.ref(om.tpe.toString), fieldName, mapping, join)
          }
        case _ => Nil
      }

    ComponentElaborator(componentMappings)
  }

  def compilerPhases: List[QueryCompiler.Phase] = List(selectElaborator, componentElaborator)

  lazy val compiler = new QueryCompiler(schema, compilerPhases)

  val interpreter: QueryInterpreter[F] = new QueryInterpreter(this)

  /** Cursor positioned at a GraphQL result leaf */
  case class LeafCursor(context: Context, focus: Any, parent: Option[Cursor], env: Env) extends Cursor {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def mkChild(context: Context = context, focus: Any = focus): LeafCursor =
      LeafCursor(context, focus, Some(this), Env.empty)

    def isLeaf: Boolean = tpe.isLeaf

    def asLeaf: Result[Json] =
      encoderForLeaf(tpe).map(enc => enc(focus).rightIor).getOrElse(mkErrorResult(
        s"Cannot encode value $focus at ${context.path.reverse.mkString("/")} (of GraphQL type ${context.tpe}). Did you forget a LeafMapping?".stripMargin.trim
      ))

    def preunique: Result[Cursor] = {
      val listTpe = tpe.nonNull.list
      focus match {
        case _: List[_] => mkChild(context.asType(listTpe), focus).rightIor
        case _ =>
          mkErrorResult(s"Expected List type, found $focus for ${listTpe}")
      }
    }

    def isList: Boolean =
      tpe match {
        case ListType(_) => true
        case _ => false
      }

    def asList[C](factory: Factory[Cursor, C]): Result[C] = (tpe, focus) match {
      case (ListType(tpe), it: List[_]) => it.view.map(f => mkChild(context.asType(tpe), focus = f)).to(factory).rightIor
      case _ => mkErrorResult(s"Expected List type, found $tpe")
    }

    def listSize: Result[Int] = (tpe, focus) match {
      case (ListType(_), it: List[_]) => it.size.rightIor
      case _ => mkErrorResult(s"Expected List type, found $tpe")
    }

    def isNullable: Boolean =
      tpe match {
        case NullableType(_) => true
        case _ => false
      }

    def asNullable: Result[Option[Cursor]] =
      (tpe, focus) match {
        case (NullableType(_), None) => None.rightIor
        case (NullableType(tpe), Some(v)) => Some(mkChild(context.asType(tpe), focus = v)).rightIor
        case _ => mkErrorResult(s"Not nullable at ${context.path}")
      }

    def isDefined: Result[Boolean] =
      (tpe, focus) match {
        case (NullableType(_), opt: Option[_]) => opt.isDefined.rightIor
        case _ => mkErrorResult(s"Not nullable at ${context.path}")
      }

    def narrowsTo(subtpe: TypeRef): Boolean = false
    def narrow(subtpe: TypeRef): Result[Cursor] =
      mkErrorResult(s"Cannot narrow $tpe to $subtpe")

    def hasField(fieldName: String): Boolean = false
    def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      mkErrorResult(s"Cannot select field '$fieldName' from leaf type $tpe")
  }
}

abstract class ComposedMapping[F[_]](implicit val M: Monad[F]) extends Mapping[F] {
  override def mkCursorForField(parent: Cursor, fieldName: String, resultName: Option[String]): Result[Cursor] = {
    val context = parent.context
    val fieldContext = context.forFieldOrAttribute(fieldName, resultName)
    fieldMapping(context, fieldName) match {
      case Some(_) =>
        ComposedCursor(fieldContext, parent.env).rightIor
      case _ =>
        super.mkCursorForField(parent, fieldName, resultName)
    }
  }

  case class ComposedCursor(context: Context, env: Env) extends AbstractCursor {
    val focus = null
    val parent = None

    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    override def hasField(fieldName: String): Boolean =
      fieldMapping(context, fieldName).isDefined

    override def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      mkCursorForField(this, fieldName, resultName)
  }
}
