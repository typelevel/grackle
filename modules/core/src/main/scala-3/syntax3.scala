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

import cats.syntax.all._
import org.typelevel.literally.Literally
import grackle.Ast.Document

trait VersionSpecificSyntax:

  extension (inline ctx: StringContext)
    inline def schema(inline args: Any*): Schema = ${SchemaLiteral('ctx, 'args)}
    inline def doc(inline args: Any*): Document = ${DocumentLiteral('ctx, 'args) }

object SchemaLiteral extends Literally[Schema]:
  def validate(s: String)(using Quotes) =
    Schema(s, CompiletimeParsers.schemaParser).toEither.bimap(
      nec => s"Invalid schema:${nec.toList.distinct.mkString("\n  🐞 ", "\n  🐞 ", "\n")}",
      _   => '{Schema(${Expr(s)}, CompiletimeParsers.schemaParser).toOption.get}
    )

object DocumentLiteral extends Literally[Document]:
  def validate(s: String)(using Quotes) =
    CompiletimeParsers.parser.parseText(s).toEither.bimap(
      _.fold(thr => show"Invalid document: ${thr.getMessage}", _.toList.mkString("\n  🐞 ", "\n  🐞 ", "\n")),
      _ => '{CompiletimeParsers.parser.parseText(${Expr(s)}).toOption.get}
    )

object CompiletimeParsers:
  val parser: GraphQLParser = GraphQLParser(GraphQLParser.defaultConfig)
  val schemaParser: SchemaParser = SchemaParser(parser)
