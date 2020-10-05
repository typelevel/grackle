// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.Monad
import cats.implicits._
import io.circe.{Encoder, Json}
import Query.Select
import QueryCompiler.{ComponentElaborator, SelectElaborator}
import QueryInterpreter.mkErrorResult
import cats.data.Chain

trait Mapping[F[_]] {
  implicit val M: Monad[F]

  val schema: Schema
  val typeMappings: List[TypeMapping]
  
  def typeMapping(tpe: Type): List[TypeMapping] =
    typeMappings.filter(_.tpe.nominal_=:=(tpe))

  def validate: Chain[Json] = {
    val typeMappingNames: List[String] = typeMappings.flatMap(_.tpe.asNamed.toList.map(_.name))

    def typeMappingFieldNames: List[String] = typeMappings.collect {
      case om: ObjectMapping => om.fieldMappings.flatMap {
        case _: CursorField[_] => Nil
        case _: CursorAttribute[_] => Nil
        case fm => List(fm.fieldName)
      }
    }.flatten

    val mappingTypesExistInSchema: List[String] =
      typeMappingNames.filterNot(name => schema.types.map(_.name).contains(name))

    val mappingFieldsExistInSchema: List[String] =
      typeMappingFieldNames.filterNot(name => schema.types.collect { case t: TypeWithFields => t.fields.map(_.name) }.flatten.contains(name))

    if (mappingTypesExistInSchema.isEmpty && mappingFieldsExistInSchema.isEmpty) Chain.empty
    else Chain(Json.fromString(
      s"Schema is missing ${mappingTypesExistInSchema.map(t => s"type: $t").mkString(", ")} ${mappingFieldsExistInSchema.map(t => s"field: $t").mkString(", ")}"))
  }

  def objectMapping(tpe: RootedType): Option[ObjectMapping] =
    (tpe.path, typeMapping(tpe.tpe)) match {
      case (_, Nil) => None
      case (Nil, List(om: ObjectMapping)) => Some(om)
      case (Nil, _) => None
      case (path, ms) =>
        ms.collectFirst {
          case om: RootedObjectMapping if om.path == path => om
        }.orElse(
          ms.collectFirst {
            case om: RootedObjectMapping if om.path.isEmpty => om
            case om: ObjectMapping => om
          }
        )
    }

  def rootMapping(tpe: Type, fieldName: String): Option[RootMapping] =
    tpe match {
      case JoinType(componentName, _) =>
        rootMapping(schema.queryType, componentName)
      case _ =>
        fieldMapping(RootedType(tpe), fieldName) match {
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

  def fieldMapping(tpe: RootedType, fieldName: String): Option[FieldMapping] =
    objectMapping(tpe).flatMap(_.fieldMappings.find(_.fieldName == fieldName).orElse {
      tpe.tpe.dealias match {
        case ot: ObjectType =>
          ot.interfaces.collectFirstSome(nt => fieldMapping(tpe.copy(tpe = nt), fieldName))
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

  trait RootedObjectMapping extends ObjectMapping {
    def path: List[String]
  }

  object ObjectMapping {
    case class DefaultObjectMapping(tpe: Type, fieldMappings: List[FieldMapping], path: List[String]) extends RootedObjectMapping

    def apply(tpe: Type, fieldMappings: List[FieldMapping], path: List[String] = Nil): ObjectMapping =
      DefaultObjectMapping(tpe, fieldMappings.map(_.withParent(tpe)), path)
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
