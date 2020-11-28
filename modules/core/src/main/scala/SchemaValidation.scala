// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import cats.Order
import cats.implicits._

trait SchemaValidation { self: Mapping[?[_]] =>

  // subclasses add methods via syntax :-\
  object Validation {
    import SchemaValidation._

    def warning(tpe: Type, field: Option[String], message: String, provider: String): Failure =
        new Failure(Severity.Warning, tpe, field, provider) {
          override def text =
            s"""|${super.text}
                |$message
                |""".stripMargin
        }

    def error(tpe: Type, field: Option[String], message: String, provider: String): Failure =
      new Failure(Severity.Error, tpe, field, provider) {
          override def text =
            s"""|${super.text}
                |$message
                |""".stripMargin
      }

    def noMapping(tpe: Type): Failure = {
      val message =
        s"""|- GraphQL type ${tpe} appears in a schema defined at ...
            |
            |However
            |
            |- No mapping for GraphQL type ${tpe} is defined in the schema mapping defined at ...
            """.stripMargin.trim
      error(tpe, None, message, getClass.getSimpleName)
    }

  }

}

object SchemaValidation {

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

  class Failure(
    val severity: Severity,
    val tpe:      Type,
    val field:    Option[String],
    val provider: String,
  ) {

    def text: String =
      s"""|$severity regarding the mapping for GraphQL schema element $tpe${field.foldMap("." + _)}:
          |""".stripMargin

    private val prefix: String =
      severity match {
        case Severity.Error   => "🔥 "
        case Severity.Warning => "⚠️ "
        case Severity.Info    => "ℹ️  "
      }

    def toErrorMessage: String =
      s"""|$text
          |This message was provided by class $provider.
          |""".stripMargin.linesIterator.mkString(s"$prefix\n$prefix", s"\n$prefix", s"\n$prefix\n")

  }

  object Failure {

    implicit val EncoderFailure: Encoder[Failure] = f =>
      Json.obj(
        "severity" -> f.severity.productPrefix.toString.asJson,
        "tpe"      -> f.tpe.toString.asJson,
        "field"    -> f.field.asJson,
        "text"     -> f.text.asJson,
      )

  }

}