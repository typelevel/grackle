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
  }

  /** Can't validate this kind of `FieldMapping`. */
  case class CannotValidateFieldMapping(owner: ObjectType, field: Field, fieldMapping: FieldMapping)
    extends Failure(Severity.Info, owner, Some(fieldMapping.fieldName)) {
    override def toString: String =
      s"$productPrefix($owner.${field.name}:${field.tpe}, ${fieldMapping.productPrefix})"
  }

  /** Object type `owner` declares `field` but no such mapping exists. */
  case class MissingFieldMapping(owner: ObjectType, field: Field)
    extends Failure(Severity.Error, owner, Some(field.name)) {
    override def toString: String =
      s"$productPrefix($owner.${field.name}:${field.tpe})"
  }

  /** GraphQL type isn't applicable for mapping type. */
  case class InapplicableGraphQLType(typeMapping: TypeMapping, actual: Type)
    extends Failure(Severity.Error, typeMapping.tpe, None) {
    override def toString: String =
      s"$productPrefix(${typeMapping.productPrefix}, ${actual.productPrefix})"
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
      case other            => Chain(InapplicableGraphQLType(lm, other))
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
              case None     => Chain(MissingFieldMapping(ot, f))
            }
          }
          case other =>
            Chain(InapplicableGraphQLType(m, other))
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

    def text: String =
      s"""|$severity regarding the mapping for $BLUE$graphQLTypeName${fieldName.foldMap("." + _)}$RESET (GraphQL):
          |""".stripMargin

    val prefix: String =
      severity match {
        case Severity.Error   => "🛑 "
        case Severity.Warning => "⚠️ "
        case Severity.Info    => "ℹ️  "
      }

    def toErrorMessage: String =
      s"""|$text
          |""".stripMargin.linesIterator.mkString(s"$prefix\n$prefix", s"\n$prefix", s"\n$prefix\n")

  }

}