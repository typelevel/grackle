// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package grackle

import scala.io.AnsiColor
import scala.util.control.NoStackTrace

import cats._
import cats.data.NonEmptyList
import cats.implicits._

import ValidationFailure.Severity

abstract class ValidationFailure(val severity: Severity) extends AnsiColor {
  protected def formattedMessage: String

  private val prefix: String =
    severity match {
      case Severity.Error   => "🛑 "
      case Severity.Warning => "⚠️ "
      case Severity.Info    => "ℹ️  "
    }

  protected def graphql(a: Any) = s"$BLUE$a$RESET"
  protected def scala(a: Any) = s"$RED$a$RESET"
  protected def key: String =
    s"Color Key: ${scala("◼")} Scala | ${graphql("◼")} GraphQL"


  final def toErrorMessage: String =
    s"""|$formattedMessage
        |$key
        |""".stripMargin.linesIterator.mkString(s"$prefix\n$prefix", s"\n$prefix", s"\n$prefix\n")

  protected def typeKind(tpe: Type): String =
    tpe.dealias match {
      case _: ObjectType => "object type"
      case _: InterfaceType => "interface type"
      case _: UnionType => "union type"
      case _: EnumType => "enum type"
      case _: ScalarType => "scalar type"
      case _ => "type"
    }

  protected def showType(tpe: Type): String = SchemaRenderer.renderType(tpe)
  protected def showNamedType(tpe: Type): String = tpe.underlyingNamed.name
}

object ValidationFailure {
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
}

final case class ValidationException(failures: NonEmptyList[ValidationFailure]) extends RuntimeException with NoStackTrace {
  override def getMessage(): String =
    s"\n\n${failures.foldMap(_.toErrorMessage)}\n"
}
