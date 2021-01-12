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
  case class InconsistentTypeMapping(owner: ObjectType, field: Field, sf: SqlField, lm: LeafMapping[_]) extends Failure(Severity.Error, owner, Some(sf.fieldName)) {
    override def toString() =
      s"$productPrefix(${owner.name}.${sf.fieldName}, ${sf.columnRef.table}.${sf.columnRef.column}:${sf.columnRef.scalaTypeName}, ${lm.tpe}:${lm.scalaTypeName})"
    override def formattedMessage: String =
      s"""|Inconsistent type mapping.
          |
          |- Field ${graphql(s"$owner.${field.name}")} of type ${graphql(field.tpe)} is defined by a Schema at (1).
          |- A ${lm.productPrefix} at (2) maps ${graphql(field.tpe)} to Scala type ${scala(lm.scalaTypeName)}.
          |- The ${scala(sf.productPrefix)} at (3) and ${scala(sf.columnRef.productPrefix)} for ${sql(s"${sf.columnRef.table}.${sf.columnRef.column}")} at (4) map ${graphql(field.tpe)} to Scala type ${scala(sf.columnRef.scalaTypeName)}.
          |- ${UNDERLINED}The Scala types are inconsistent.$RESET
          |
          |(1) ${schema.pos}
          |(2) ${lm.pos}
          |(3) ${sf.pos}
          |(4) ${sf.columnRef.pos}
          |""".stripMargin
  }

  /** An expected LeafMapping was not found. */
  case class MissingLeafMapping(owner: ObjectType, field: Field, sf: SqlField) extends Failure(Severity.Error, owner, Some(sf.fieldName)) {
    override def toString() =
      s"$productPrefix(${owner.name}.${sf.fieldName}, ${sf.columnRef.table}.${sf.columnRef.column}:${sf.columnRef.scalaTypeName})"
    override def formattedMessage: String =
      s"""|Missing leaf mapping.
          |
          |- Field ${graphql(s"$owner.${field.name}")} of type ${graphql(field.tpe)} is defined by a Schema at (1).
          |- ${UNDERLINED}No leaf mapping was found$RESET for ${graphql(field.tpe)}.
          |- The ${scala(sf.productPrefix)} at (2) and ${scala(sf.columnRef.productPrefix)} for ${sql(s"${sf.columnRef.table}.${sf.columnRef.column}")} at (3) suggest that ${graphql(field.tpe)} should map to Scala type ${scala(sf.columnRef.scalaTypeName)}.
          |
          |(1) ${schema.pos}
          |(2) ${sf.pos}
          |(2) ${sf.columnRef.pos}
          |""".stripMargin
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
                else Chain(InconsistentTypeMapping(owner, field, sf, lm))
              case _ => Chain(MissingLeafMapping(owner, field, sf))
            }

          case _ => super.validateFieldMapping(owner, field, fieldMapping)

        }



      case other => super.validateFieldMapping(owner, field, other)
    }

}