// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.Monad
import cats.implicits._
import io.circe.Encoder

import Query.Select
import QueryCompiler.{ComponentElaborator, SelectElaborator}
import QueryInterpreter.mkErrorResult

trait Mapping[F[_]] {
  implicit val M: Monad[F]

  val schema: Schema
  val typeMappings: List[TypeMapping]

  def typeMapping(tpe: Type): Option[TypeMapping] =
    typeMappings.find(_.tpe.nominal_=:=(tpe))

  def objectMapping(tpe: Type): Option[ObjectMapping] =
    typeMapping(tpe) match {
      case Some(om: ObjectMapping) => Some(om)
      case _ => None
    }

  def rootMapping(tpe: Type, fieldName: String): Option[RootMapping] =
    tpe match {
      case JoinType(componentName, _) =>
        rootMapping(schema.queryType, componentName)
      case _ =>
        fieldMapping(tpe, fieldName) match {
          case Some(rm: RootMapping) => Some(rm)
          case _ => None
        }
    }

  def rootCursor(rootTpe: Type, fieldName: String, child: Query): F[Result[Cursor]] =
    rootMapping(rootTpe, fieldName) match {
      case Some(root) =>
        root.cursor(child)
      case None =>
        mkErrorResult(s"No root field '$fieldName' in $rootTpe").pure[F]
    }

  def fieldMapping(tpe: Type, fieldName: String): Option[FieldMapping] =
    objectMapping(tpe).flatMap(_.fieldMappings.find(_.fieldName == fieldName).orElse {
      tpe.dealias match {
        case ot: ObjectType =>
          ot.interfaces.collectFirstSome(nt => fieldMapping(nt, fieldName))
        case _ => None
      }
    })

  def leafMapping[T](tpe: Type): Option[LeafMapping[T]] =
    typeMappings.collectFirst {
      case lm@LeafMapping(tpe0, _) if tpe0 =:= tpe => lm.asInstanceOf[LeafMapping[T]]
    }

  trait TypeMapping extends Product with Serializable {
    def tpe: Type
  }

  trait ObjectMapping extends TypeMapping {
    def fieldMappings: List[FieldMapping]
  }
  object ObjectMapping {
    case class DefaultObjectMapping(tpe: Type, fieldMappings: List[FieldMapping]) extends ObjectMapping

    def apply(tpe: Type, fieldMappings: List[FieldMapping]): ObjectMapping =
      DefaultObjectMapping(tpe, fieldMappings.map(_.withParent(tpe)))
  }

  trait FieldMapping extends Product with Serializable {
    def fieldName: String
    def withParent(tpe: Type): FieldMapping
  }

  trait RootMapping extends FieldMapping {
    def cursor(query: Query): F[Result[Cursor]]
    def withParent(tpe: Type): RootMapping
  }

  trait LeafMapping[T] extends TypeMapping {
    def tpe: Type
    def encoder: Encoder[T]
  }
  object LeafMapping {
    case class DefaultLeafMapping[T](tpe: Type, encoder: Encoder[T]) extends LeafMapping[T]

    def apply[T](tpe: Type)(implicit encoder: Encoder[T]): LeafMapping[T] =
      DefaultLeafMapping(tpe, encoder)

    def unapply[T](lm: LeafMapping[T]): Option[(Type, Encoder[T])] =
      Some((lm.tpe, lm.encoder))
  }

  case class CursorField[T](fieldName: String, f: Cursor => Result[T], encoder: Encoder[T]) extends FieldMapping {
    def withParent(tpe: Type): CursorField[T] = this
  }
  object CursorField {
    def apply[T](fieldName: String, f: Cursor => Result[T])(implicit encoder: Encoder[T], di: DummyImplicit): CursorField[T] =
      new CursorField(fieldName, f, encoder)
  }

  case class CursorAttribute[T](fieldName: String, f: Cursor => Result[T]) extends FieldMapping {
    def withParent(tpe: Type): CursorAttribute[T] = this
  }

  case class Delegate(
    fieldName: String,
    interpreter: Mapping[F],
    join: (Cursor, Query) => Result[Query] = ComponentElaborator.TrivialJoin
  ) extends FieldMapping {
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

abstract class AbstractMapping[+M[f[_]] <: Monad[f], F[_]](implicit val M: M[F]) extends Mapping[F]

trait SimpleMapping[F[_]] extends AbstractMapping[Monad, F]
