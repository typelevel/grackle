// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package sql

import cats.data.Chain
import org.tpolecat.typename.typeName

class SqlMappingValidator[M <: SqlMapping[F] forSome { type F[a] }](sqlMapping: M) extends MappingValidator[M](sqlMapping) {
  import MappingValidator._
  import mapping._

  /** SqlField codec and LeafMapping are inconsistent. */
  case class InconsistentTypeMapping(owner: ObjectType, sf: SqlField, lm: LeafMapping[_]) extends Failure(Severity.Error, owner, Some(sf.fieldName)) {
    override def toString() =
      s"$productPrefix(${owner.name}.${sf.fieldName}, ${sf.columnRef.table}.${sf.columnRef.column}:${sf.columnRef.scalaTypeName}, ${lm.tpe}:${lm.scalaTypeName})"
  }

  /** An expected LeafMapping was not found. */
  case class MissingLeafMapping(owner: ObjectType, sf: SqlField) extends Failure(Severity.Error, owner, Some(sf.fieldName)) {
    override def toString() =
      s"$productPrefix(${owner.name}.${sf.fieldName}, ${sf.columnRef.table}.${sf.columnRef.column}:${sf.columnRef.scalaTypeName})"
  }

  override protected def validateFieldMapping(owner: ObjectType, field: Field, fieldMapping: mapping.FieldMapping): Chain[MappingValidator.Failure] =
    fieldMapping match {
      case sf @ SqlField(_, columnRef, _, _) =>

        field.tpe.dealias match {

          case ScalarType.BooleanType if columnRef.scalaTypeName == typeName[Boolean] => Chain.empty
          case ScalarType.FloatType   if columnRef.scalaTypeName == typeName[Double]  => Chain.empty
          case ScalarType.StringType  if columnRef.scalaTypeName == typeName[String]  => Chain.empty
          case ScalarType.IDType      if columnRef.scalaTypeName == typeName[String]  => Chain.empty
          case ScalarType.IntType     if columnRef.scalaTypeName == typeName[Int]     => Chain.empty

          case NullableType(ScalarType.BooleanType) if columnRef.scalaTypeName == typeName[Option[Boolean]] => Chain.empty
          case NullableType(ScalarType.FloatType)   if columnRef.scalaTypeName == typeName[Option[Double]]  => Chain.empty
          case NullableType(ScalarType.StringType)  if columnRef.scalaTypeName == typeName[Option[String]]  => Chain.empty
          case NullableType(ScalarType.IDType)      if columnRef.scalaTypeName == typeName[Option[String]]  => Chain.empty
          case NullableType(ScalarType.IntType)     if columnRef.scalaTypeName == typeName[Option[Int]]     => Chain.empty

          case tpe @ ScalarType(_, _) =>
            typeMapping(tpe) match {
              case Some(lm: LeafMapping[_]) =>
                if (lm.scalaTypeName == columnRef.scalaTypeName) Chain.empty
                else Chain(InconsistentTypeMapping(owner, sf, lm))
              case _ => Chain(MissingLeafMapping(owner, sf))
            }

          case _ => super.validateFieldMapping(owner, field, fieldMapping)

        }



      case other => super.validateFieldMapping(owner, field, other)
    }

}