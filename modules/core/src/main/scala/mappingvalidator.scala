// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats._
import cats.data.Chain
import cats.implicits._
import scala.io.AnsiColor

class MappingValidator[M <: Mapping[F] forSome { type F[a] }](val mapping: M) {
  import MappingValidator._
  import mapping._

  /** Can't validate this kind of `FieldMapping`. */
  case class CannotValidateTypeMapping(typeMapping: TypeMapping)
    extends Failure(Severity.Info, typeMapping.tpe, None) {
    override def toString: String =
      s"$productPrefix(${typeMapping.tpe}, ${typeMapping.productPrefix})"
    override def formattedMessage: String =
      s"""|The ${typeMapping.productPrefix} for ${typeMapping.tpe} cannot be validated.
          |""".stripMargin
  }

  /** Can't validate this kind of `FieldMapping`. */
  case class CannotValidateFieldMapping(owner: ObjectType, field: Field, fieldMapping: FieldMapping)
    extends Failure(Severity.Info, owner, Some(fieldMapping.fieldName)) {
    override def toString: String =
      s"$productPrefix($owner.${field.name}:${field.tpe}, ${fieldMapping.productPrefix})"
    override def formattedMessage: String =
      s"""|Field mapping cannot be validated.
          |
          |- Field ${graphql(s"$owner.${field.name}: ${field.tpe}")} is defined by a Schema at (1).
          |- Its mapping to Scala is defined by a ${scala(fieldMapping.productPrefix)} at (2).
          |- ${UNDERLINED}This kind of mapping canont be validated.$RESET Ensure you have unit tests.
          |
          |(1) ${schema.pos}
          |(2) ${fieldMapping.pos}
          |""".stripMargin
  }

  /** Object type `owner` declares `field` but no such mapping exists. */
  case class MissingFieldMapping(owner: ObjectMapping, field: Field)
    extends Failure(Severity.Error, owner.tpe, Some(field.name)) {
    override def toString: String =
      s"$productPrefix($owner.${field.name}:${field.tpe})"
    override def formattedMessage: String =
      s"""|Missing field mapping.
          |
          |- Field ${graphql(s"${owner.tpe}${field.name}: ${field.tpe}")} is defined by a Schema at (1).
          |- The ${scala(owner.productPrefix)} for ${graphql(owner.tpe)} at (2) ${UNDERLINED}does not define a mapping for this field$RESET.
          |
          |(1) ${schema.pos}
          |(2) ${owner.pos}
          |""".stripMargin
  }

  /** GraphQL type isn't applicable for mapping type. */
  case class InapplicableGraphQLType(typeMapping: TypeMapping, expected: String)
    extends Failure(Severity.Error, typeMapping.tpe, None) {
    override def toString: String =
      s"$productPrefix(${typeMapping.productPrefix}, ${typeMapping.tpe.productPrefix})"
    override def formattedMessage: String =
      s"""|Inapplicable GraphQL type.
          |
          |- Type ${graphql(typeMapping.tpe)} (${scala(typeMapping.tpe.dealias.productPrefix)}) is defined by a Schema at (1).
          |- It is mapped by a ${graphql(typeMapping.productPrefix)} at (2), which expects ${scala(expected)}.
          |- ${UNDERLINED}Use a different kind of mapping for this type.$RESET
          |
          |(1) ${schema.pos}
          |(2) ${typeMapping.pos}
          |""".stripMargin
  }


  def validateMapping(): Chain[Failure] =
    Chain.fromSeq(typeMappings).foldMap(validateTypeMapping)

  protected def validateTypeMapping(tm: TypeMapping): Chain[Failure] =
    tm match {
      case om: ObjectMapping  => validateObjectMapping(om)
      case lm: LeafMapping[_] => validateLeafMapping(lm)
      case tm                 => Chain(CannotValidateTypeMapping(tm))
    }

  protected def validateLeafMapping(lm: LeafMapping[_]): Chain[Failure] =
    lm.tpe.dealias match {
      case ScalarType(_, _) => Chain.empty // these are valid on construction. Nothing to do.
      case _                => Chain(InapplicableGraphQLType(lm, "ScalarType"))
    }

  protected def validateFieldMapping(owner: ObjectType, field: Field, fieldMapping: FieldMapping): Chain[Failure] =
    Chain(CannotValidateFieldMapping(owner, field, fieldMapping))

  protected def validateObjectMapping(m: ObjectMapping): Chain[Failure] =
    m match {
      case ObjectMapping.DefaultObjectMapping(tpe, fieldMappings) =>
        tpe.dealias match {
          case ot @ ObjectType(_, _, fields, _) => fields.foldMap { f =>
            fieldMappings.find(_.fieldName == f.name) match {
              case Some(fm) => validateFieldMapping(ot, f, fm)
              case None     => Chain(MissingFieldMapping(m, f))
            }
          }
          case _ =>
            Chain(InapplicableGraphQLType(m, "ObjectType"))
        }
    }

}

object MappingValidator {

  sealed trait Severity extends Product
  object Severity {
    case object  Error   extends Severity
    case object  Warning extends Severity
    case object  Info    extends Severity

    implicit val OrderSeverity: Order[Severity] =
      Order.by {
        case Error   => 1
        case Warning => 2
        case Info    => 3
      }

  }

  abstract class Failure(
    val severity: Severity,
    val graphQLTypeName: String,
    val fieldName:    Option[String],
  ) extends AnsiColor {

    def this(
      severity: Severity,
      tpe:      Type,
      fieldName:    Option[String],
    ) = this(severity, tpe.toString, fieldName)

    def formattedMessage: String = s"$toString (no detail given)"

    val prefix: String =
      severity match {
        case Severity.Error   => "🛑 "
        case Severity.Warning => "⚠️ "
        case Severity.Info    => "ℹ️  "
      }

    def graphql(a: Any) = s"$BLUE$a$RESET"
    def scala(a: Any) = s"$RED$a$RESET"
    def sql(a: Any) = s"$GREEN$a$RESET"

    final def toErrorMessage: String =
      s"""|$formattedMessage
          |
          |Color Key: ${scala("◼")} Scala | ${graphql("◼")} GraphQL | ${sql("◼")} SQL
          |""".stripMargin.linesIterator.mkString(s"$prefix\n$prefix", s"\n$prefix", s"\n$prefix\n")

  }

}