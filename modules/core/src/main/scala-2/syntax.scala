// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.syntax.all._
import org.typelevel.literally.Literally
import edu.gemini.grackle.Schema
import io.circe.Json
import io.circe.parser.parse

object syntax {

  implicit class StringContextOps(val sc: StringContext) extends AnyVal {
    def schema(args: Any*): Schema = macro SchemaLiteral.make
    def json(args: Any*): Json = macro JsonLiteral.make
  }

  object SchemaLiteral extends Literally[Schema] {
    def validate(c: Context)(s: String): Either[String,c.Expr[Schema]] = {
      import c.universe._
      Schema(s).toEither.bimap(
        nec => s"Invalid schema:${nec.toList.distinct.mkString("\n  🐞 ", "\n  🐞 ", "\n")}",
        _ => c.Expr(q"_root_.edu.gemini.grackle.Schema($s).right.get")
      )
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

}