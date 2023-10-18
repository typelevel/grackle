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

import cats.data.NonEmptyChain
import cats.syntax.all._
import org.typelevel.literally.Literally
import grackle.Ast.Document
import grackle.GraphQLParser.Document.parseAll
import grackle.Schema

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
    Schema(s).toEither.bimap(mkError, _ => c.Expr(q"_root_.grackle.Schema($s).toOption.get"))
  }
  def make(c: Context)(args: c.Expr[Any]*): c.Expr[Schema] = apply(c)(args: _*)
}

object DocumentLiteral extends Literally[Document] {
  def validate(c: Context)(s: String): Either[String,c.Expr[Document]] = {
    import c.universe._
    parseAll(s).bimap(
      pf => show"Invalid document: $pf",
      _  => c.Expr(q"_root_.grackle.GraphQLParser.Document.parseAll($s).toOption.get"),
    )
  }
  def make(c: Context)(args: c.Expr[Any]*): c.Expr[Document] = apply(c)(args: _*)
}
