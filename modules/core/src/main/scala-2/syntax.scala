// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.data.NonEmptyChain
import cats.syntax.all._
import org.typelevel.literally.Literally
import edu.gemini.grackle.Schema
import io.circe.Json
import io.circe.parser.parse

trait VersionSpecificSyntax {
  implicit def toStringContextOps(sc: StringContext): StringContextOps =
    new StringContextOps(sc)
}

class StringContextOps(val sc: StringContext) extends AnyVal {
  def schema(args: Any*): Schema = macro SchemaLiteral.make
  def json(args: Any*): Json = macro JsonLiteral.make
}

object SchemaLiteral extends Literally[Schema] {
  def validate(c: Context)(s: String): Either[String,c.Expr[Schema]] = {
    import c.universe._
    def mkError(err: Either[Throwable, NonEmptyChain[Problem]]) =
      err.fold(
        t  => s"Internal error: ${t.getMessage}",
        ps => s"Invalid schema: ${ps.toList.distinct.mkString("\n  🐞 ", "\n  🐞 ", "\n")}",
      )
    Schema(s).toEither.bimap(mkError, _ => c.Expr(q"_root_.edu.gemini.grackle.Schema($s).toOption.get"))
  }
  def make(c: Context)(args: c.Expr[Any]*): c.Expr[Schema] = apply(c)(args: _*)
}

object JsonLiteral extends Literally[Json] {
  def validate(c: Context)(s: String): Either[String,c.Expr[Json]] = {
    import c.universe._
    parse(s).bimap(
      pf => s"Invalid JSON: ${pf.message}",
      _  => c.Expr(q"_root_.io.circe.parser.parse($s).toOption.get"),
    )
  }
  def make(c: Context)(args: c.Expr[Any]*): c.Expr[Json] = apply(c)(args: _*)
}
