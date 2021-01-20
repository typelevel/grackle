// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.Monad
import cats.data.Ior
import cats.implicits._
import io.circe.{Encoder, Json}

import Query.Select
import QueryCompiler.{ComponentElaborator, SelectElaborator}
import QueryInterpreter.mkErrorResult
import org.tpolecat.typename._
import org.tpolecat.sourcepos.SourcePos

trait QueryExecutor[F[_], T] { outer =>
  implicit val M: Monad[F]

  def run(query: Query, rootTpe: Type): F[T]

  def compileAndRun(text: String, name: Option[String] = None, untypedEnv: Option[Json] = None, useIntrospection: Boolean = true): F[T]
}

abstract class Mapping[F[_]](implicit val M: Monad[F]) extends QueryExecutor[F, Json] {
  val schema: Schema
  val typeMappings: List[TypeMapping]

  def run(query: Query, rootTpe: Type): F[Json] =
    interpreter.run(query, rootTpe)

  def run(op: Operation): F[Json] =
    run(op.query, op.rootTpe)

  def compileAndRun(text: String, name: Option[String] = None, untypedEnv: Option[Json] = None, useIntrospection: Boolean = true): F[Json] =
    compiler.compile(text, name, untypedEnv, useIntrospection) match {
      case Ior.Right(operation) =>
        run(operation.query, schema.queryType)
      case invalid =>
        QueryInterpreter.mkInvalidResponse(invalid).pure[F]
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

  def rootCursor(path: List[String], rootTpe: Type, fieldName: String, child: Query): F[Result[Cursor]] =
    rootMapping(path, rootTpe, fieldName) match {
      case Some(root) =>
        root.cursor(child)
      case None =>
        mkErrorResult(s"No root field '$fieldName' in $rootTpe").pure[F]
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
    def isPublic: Boolean
    def withParent(tpe: Type): FieldMapping
    def pos: SourcePos
  }

  trait RootMapping extends FieldMapping {
    def isPublic = true
    def cursor(query: Query): F[Result[Cursor]]
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

  case class CursorField[T](fieldName: String, f: Cursor => Result[T], encoder: Encoder[T], required: List[String])(
    implicit val pos: SourcePos
  ) extends FieldMapping {
    def isPublic = true
    def withParent(tpe: Type): CursorField[T] = this
  }
  object CursorField {
    def apply[T](fieldName: String, f: Cursor => Result[T], required: List[String] = Nil)(implicit encoder: Encoder[T], di: DummyImplicit): CursorField[T] =
      new CursorField(fieldName, f, encoder, required)
  }

  case class CursorAttribute[T](fieldName: String, f: Cursor => Result[T], required: List[String] = Nil)(
    implicit val pos: SourcePos
  ) extends FieldMapping {
    def isPublic = false
    def withParent(tpe: Type): CursorAttribute[T] = this
  }

  case class Delegate(
    fieldName: String,
    interpreter: Mapping[F],
    join: (Cursor, Query) => Result[Query] = ComponentElaborator.TrivialJoin
  )(implicit val pos: SourcePos) extends FieldMapping {
    def isPublic = true
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
