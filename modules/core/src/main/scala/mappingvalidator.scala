// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats._
import cats.data.{ Chain, NonEmptyList }
import cats.implicits._
import scala.io.AnsiColor

trait MappingValidator {

  type F[_]
  type M <: Mapping[F]

  val mapping: M

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
      s"$productPrefix(${owner.tpe}.${field.name}:${field.tpe})"
    override def formattedMessage: String =
      s"""|Missing field mapping.
          |
          |- Field ${graphql(s"${owner.tpe}.${field.name}: ${field.tpe}")} is defined by a Schema at (1).
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

  /** Referenced type does not exist. */
  case class ReferencedTypeDoesNotExist(typeMapping: TypeMapping)
    extends Failure(Severity.Error, typeMapping.tpe, None) {
    override def toString: String =
      s"$productPrefix(${typeMapping.productPrefix}, ${typeMapping.tpe})"
    override def formattedMessage: String =
      s"""|Referenced type does not exist.
          |
          |- A ${graphql(typeMapping.productPrefix)} at (1) references type ${graphql(typeMapping.tpe)}.
          |- ${UNDERLINED}This type is undeclared$RESET in referenced Schema at (2).
          |
          |(1) ${typeMapping.pos}
          |(2) ${schema.pos}
          |""".stripMargin
  }

  /** Referenced field does not exist. */
  case class ReferencedFieldDoesNotExist(objectMapping: ObjectMapping, fieldMapping: FieldMapping)
    extends Failure(Severity.Error, objectMapping.tpe, Some(fieldMapping.fieldName)) {
    override def toString: String =
      s"$productPrefix(${objectMapping.tpe}.${fieldMapping.fieldName})"
    override def formattedMessage: String =
      s"""|Referenced field does not exist.
          |
          |- ${objectMapping.tpe} is defined in a Schema at (1).
          |- A ${graphql(objectMapping.productPrefix)} at (2) references field ${graphql(fieldMapping.fieldName)}.
          |- ${UNDERLINED}This field does not exist in the Schema.$RESET
          |
          |(1) ${schema.pos}
          |(1) ${objectMapping.pos}
          |""".stripMargin
  }

  /**
   * Run this validator, yielding a chain of `Failure`s of severity equal to or greater than the
   * specified `Severity`.
   */
  def validateMapping(severity: Severity = Severity.Warning): List[Failure] =
    typeMappings.foldMap(validateTypeMapping).filter(_.severity >= severity).toList

  /**
   * Run this validator, raising a `ValidationException` in `G` if there are any failures of
   * severity equal to or greater than the specified `Severity`.
   */
  def validate[G[_]](severity: Severity = Severity.Warning)(
    implicit ev: ApplicativeError[G, Throwable]
  ): G[Unit] =
    NonEmptyList.fromList(validateMapping(severity)).foldMapA(nec => ev.raiseError(ValidationException(nec)))

  /**
   * Run this validator, raising a `ValidationException` if there are any failures of severity equal
   * to or greater than the specified `Severity`.
   */
  def unsafeValidate(severity: Severity = Severity.Warning): Unit =
    validate[Either[Throwable, *]](severity).fold(throw _, _ => ())

  protected def validateTypeMapping(tm: TypeMapping): Chain[Failure] = {
    if (!tm.tpe.dealias.exists) Chain(ReferencedTypeDoesNotExist(tm))
    else tm match {
      case om: ObjectMapping  => validateObjectMapping(om)
      case lm: LeafMapping[_] => validateLeafMapping(lm)
      case tm                 => Chain(CannotValidateTypeMapping(tm))
    }
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
          case ot @ ObjectType(_, _, fields, _) =>

            val a = fields.foldMap { f =>
              fieldMappings.find(_.fieldName == f.name) match {
                case Some(fm) => validateFieldMapping(ot, f, fm)
                case None     => Chain(MissingFieldMapping(m, f))
              }
            }

            val b = fieldMappings.foldMap { fm =>
              fields.find(_.name == fm.fieldName) match {
                case Some(_) => Chain.empty
                case None => Chain(ReferencedFieldDoesNotExist(m, fm))
              }
            }

            a ++ b

        case InterfaceType(_, _, _, _) =>
          Chain(CannotValidateTypeMapping(m))

        case _ =>
          Chain(InapplicableGraphQLType(m, "ObjectType"))
      }

      case other =>
        Chain(CannotValidateTypeMapping(other))

    }



}

object MappingValidator {

  def apply[G[_]](m: Mapping[G]): MappingValidator =
    new MappingValidator {
      type F[a] = G[a]
      type M = Mapping[F]
      val mapping = m
    }

  sealed trait Severity extends Product
  object Severity {
    case object  Error   extends Severity
    case object  Warning extends Severity
    case object  Info    extends Severity

    implicit val OrderSeverity: Order[Severity] =
      Order.by {
        case Error   => 3
        case Warning => 2
        case Info    => 1
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

    protected def formattedMessage: String = s"$toString (no detail given)"

    private val prefix: String =
      severity match {
        case Severity.Error   => "🛑 "
        case Severity.Warning => "⚠️ "
        case Severity.Info    => "ℹ️  "
      }

    protected def graphql(a: Any) = s"$BLUE$a$RESET"
    protected def scala(a: Any) = s"$RED$a$RESET"
    protected def sql(a: Any) = s"$GREEN$a$RESET"

    final def toErrorMessage: String =
      s"""|$formattedMessage
          |
          |Color Key: ${scala("◼")} Scala | ${graphql("◼")} GraphQL | ${sql("◼")} SQL
          |""".stripMargin.linesIterator.mkString(s"$prefix\n$prefix", s"\n$prefix", s"\n$prefix\n")

  }

  final case class ValidationException(failures: NonEmptyList[Failure]) extends RuntimeException {
    override def getMessage(): String =
      failures.foldMap(_.toErrorMessage)
  }

}