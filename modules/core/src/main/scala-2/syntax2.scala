// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.data.NonEmptyChain
import cats.syntax.all._
import org.typelevel.literally.Literally
import edu.gemini.grackle.Ast.Document
import edu.gemini.grackle.GraphQLParser.Document.parseAll
import edu.gemini.grackle.Schema

trait VersionSpecificSyntax {
  implicit def toStringContextOps(sc: StringContext): StringContextOps =
    new StringContextOps(sc)
}

class StringContextOps(val sc: StringContext) extends AnyVal {
  def schema(args: Any*): Schema = macro SchemaLiteral.make
  def doc(args: Any*): Document = macro DocumentLiteral.make
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

object DocumentLiteral extends Literally[Document] {
  def validate(c: Context)(s: String): Either[String,c.Expr[Document]] = {
    import c.universe._
    parseAll(s).bimap(
      pf => show"Invalid document: $pf",
      _  => c.Expr(q"_root_.edu.gemini.grackle.GraphQLParser.Document.parseAll($s).toOption.get"),
    )
  }
  def make(c: Context)(args: c.Expr[Any]*): c.Expr[Document] = apply(c)(args: _*)
}
