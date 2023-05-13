// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import cats.syntax.all._
import org.typelevel.literally.Literally
import edu.gemini.grackle.Ast.Document
import edu.gemini.grackle.GraphQLParser.Document.parseAll
import edu.gemini.grackle.Schema
import io.circe.Json
import io.circe.parser.parse

trait VersionSpecificSyntax:

  extension (inline ctx: StringContext)
    inline def schema(inline args: Any*): Schema = ${SchemaLiteral('ctx, 'args)}
    inline def json(inline args: Any*): Json = ${JsonLiteral('ctx, 'args)}
    inline def doc(inline args: Any*): Document = ${ DocumentLiteral('ctx, 'args) }

object SchemaLiteral extends Literally[Schema]:
  def validate(s: String)(using Quotes) =
    Schema(s).toEither.bimap(
      nec => s"Invalid schema:${nec.toList.distinct.mkString("\n  🐞 ", "\n  🐞 ", "\n")}",
      _   => '{Schema(${Expr(s)}).toOption.get}
    )

object JsonLiteral extends Literally[Json]:
  def validate(s: String)(using Quotes) =
    parse(s).bimap(
      pf => s"Invalid JSON: ${pf.message}",
      _  => '{parse(${Expr(s)}).toOption.get}
    )

object DocumentLiteral extends Literally[Document]:
  def validate(s: String)(using Quotes) =
    parseAll(s).bimap(
      pf => show"Invalid document: $pf",
      _ => '{parseAll(${Expr(s)}).toOption.get}
    )
