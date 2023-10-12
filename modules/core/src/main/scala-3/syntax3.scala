// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package grackle

import cats.syntax.all._
import org.typelevel.literally.Literally
import grackle.Ast.Document
import grackle.GraphQLParser.Document.parseAll

trait VersionSpecificSyntax:

  extension (inline ctx: StringContext)
    inline def schema(inline args: Any*): Schema = ${SchemaLiteral('ctx, 'args)}
    inline def doc(inline args: Any*): Document = ${ DocumentLiteral('ctx, 'args) }

object SchemaLiteral extends Literally[Schema]:
  def validate(s: String)(using Quotes) =
    Schema(s).toEither.bimap(
      nec => s"Invalid schema:${nec.toList.distinct.mkString("\n  🐞 ", "\n  🐞 ", "\n")}",
      _   => '{Schema(${Expr(s)}).toOption.get}
    )

object DocumentLiteral extends Literally[Document]:
  def validate(s: String)(using Quotes) =
    parseAll(s).bimap(
      pf => show"Invalid document: $pf",
      _ => '{parseAll(${Expr(s)}).toOption.get}
    )
