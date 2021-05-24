// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.Monad
import cats.data.{ Ior, IorT }
import cats.implicits._
import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.tpolecat.typename._
import org.tpolecat.sourcepos.SourcePos

import Cursor.Env
import Query.Select
import QueryCompiler.{ComponentElaborator, SelectElaborator, IntrospectionLevel}
import QueryInterpreter.mkErrorResult
import IntrospectionLevel._
import fs2.{ Stream, Compiler }

trait QueryExecutor[F[_], T] { outer =>

  def run(query: Query, rootTpe: Type, env: Env): Stream[F,T]

  // TODO: deprecate
  def compileAndRun(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, env: Env = Env.empty)(
    implicit sc: Compiler[F,F]
  ): F[T] =
    compileAndRunOne(text, name, untypedVars, introspectionLevel, env)

  def compileAndRunAll(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, env: Env = Env.empty): Stream[F,T]

  def compileAndRunOne(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, env: Env = Env.empty)(
    implicit sc: Compiler[F,F]
  ): F[T]

}

abstract class Mapping[F[_]](implicit val M: Monad[F]) extends QueryExecutor[F, Json] {
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

  def typeMapping(tpe: Type): Option[TypeMapping] =
    typeMappings.find(_.tpe.nominal_=:=(tpe))

  val validator: MappingValidator =
    MappingValidator(this)

  def rootMapping(path: List[String], tpe: Type, fieldName: String): Option[RootMapping] =
    tpe match {
      case JoinType(componentName, _) =>
        rootMapping(Nil, schema.queryType, componentName)
      case _ =>
        fieldMapping(path, tpe, fieldName).collect {
          case rm: RootMapping => rm
        }
    }

  def rootCursor(path: List[String], rootTpe: Type, fieldName: String, child: Query, env: Env): Stream[F,Result[(Query, Cursor)]] =
    rootMapping(path, rootTpe, fieldName) match {
      case Some(root) =>
        root.run(child, env)
      case None =>
        mkErrorResult[(Query, Cursor)](s"No root field '$fieldName' in $rootTpe").pure[Stream[F,*]]
    }

  def objectMapping(path: List[String], tpe: Type): Option[ObjectMapping] =
    typeMapping(tpe) match {
      case Some(om: ObjectMapping) => Some(om)
      case Some(pm: PrefixedMapping) =>
        val matching = pm.mappings.filter(m => path.startsWith(m._1.reverse))
        matching.sortBy(m => -m._1.length).headOption.map(_._2)
      case _ => None
    }

  def fieldMapping(path: List[String], tpe: Type, fieldName: String): Option[FieldMapping] =
    objectMapping(path, tpe).flatMap(_.fieldMappings.find(_.fieldName == fieldName).orElse {
      tpe.dealias match {
        case ot: ObjectType =>
          ot.interfaces.collectFirstSome(nt => fieldMapping(path, nt, fieldName))
        case _ => None
      }
    })

  def leafMapping[T](tpe: Type): Option[LeafMapping[T]] =
    typeMappings.collectFirst {
      case lm@LeafMapping(tpe0, _) if tpe0 =:= tpe => lm.asInstanceOf[LeafMapping[T]]
    }

  trait TypeMapping extends Product with Serializable {
    def tpe: Type
    def pos: SourcePos
  }

  case class PrimitiveMapping(tpe: Type)(implicit val pos: SourcePos) extends TypeMapping

  trait ObjectMapping extends TypeMapping {
    def fieldMappings: List[FieldMapping]
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

  /**
   * Root mappings can perform a mutation prior to constructing the result `Cursor`. A `Mutation`
   * may perform a Unit effect and simply return the passed arguments; or it may refine the passed
   * `Query` and/or `Env` that will be used to interpret the resulting `Cursor`.
   */
  case class Mutation(run: (Query, Env) => Stream[F,Result[(Query, Env)]])
  object Mutation {

    /** The no-op mutation. */
    val None: Mutation =
      Mutation((q, e) => Result((q, e)).pure[Stream[F,*]])

    /** A mutation that peforms a Unit effect and yields its arguments unchanged. */
    def unit(f: (Query, Env) => Stream[F, Result[Unit]]): Mutation =
      Mutation((q, e) => f(q, e).map(_.as((q, e))))

  }


  trait RootMapping extends FieldMapping {
    def mutation: Mutation

    /**
     * Run this `RootMapping`'s mutation, if any, then construct and return the result cursor along
     * with the [possibly updated] query.
     */
    final def run(query: Query, env: Env): Stream[F, Result[(Query, Cursor)]] =
      IorT(mutation.run(query, env)).flatMap { case (q, e) =>
        IorT(cursor(q, e)).tupleLeft(q)
      } .value

    def hidden = false
    def cursor(query: Query, env: Env): Stream[F, Result[Cursor]]
    def withParent(tpe: Type): RootMapping
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
}
